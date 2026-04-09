//! Graph Transformer Value Network for Qwirkle.
//!
//! Adapted from the Take it Easy Graph Transformer architecture.
//! Full self-attention over board nodes (occupied + candidate cells)
//! with attention masking for variable-size boards padded to MAX_NODES.

#![cfg(feature = "neural")]

use tch::{nn, Device, Kind, Tensor};

pub const MAX_NODES: i64 = 128;
pub const INPUT_DIM: i64 = 22;
pub const D_MODEL: i64 = 64;
pub const N_HEADS: i64 = 4;
pub const N_LAYERS: usize = 4;
pub const D_FF: i64 = 256;
pub const DROPOUT: f64 = 0.1;
pub const CONTEXT_DIM: i64 = 4;

// ── Multi-Head Self-Attention ──

pub struct MultiHeadAttention {
    q_proj: nn::Linear,
    k_proj: nn::Linear,
    v_proj: nn::Linear,
    out_proj: nn::Linear,
    num_heads: i64,
    head_dim: i64,
    scale: f64,
}

impl MultiHeadAttention {
    pub fn new(path: &nn::Path, embed_dim: i64, num_heads: i64) -> Self {
        let head_dim = embed_dim / num_heads;
        Self {
            q_proj: nn::linear(path / "q_proj", embed_dim, embed_dim, Default::default()),
            k_proj: nn::linear(path / "k_proj", embed_dim, embed_dim, Default::default()),
            v_proj: nn::linear(path / "v_proj", embed_dim, embed_dim, Default::default()),
            out_proj: nn::linear(path / "out_proj", embed_dim, embed_dim, Default::default()),
            num_heads,
            head_dim,
            scale: 1.0 / (head_dim as f64).sqrt(),
        }
    }

    /// Forward pass with optional attention mask.
    /// mask shape: [batch, max_nodes] with true for valid nodes, false for padding.
    pub fn forward(&self, x: &Tensor, mask: Option<&Tensor>, train: bool, dropout: f64) -> Tensor {
        let (batch, seq, _) = x.size3().unwrap();

        let q = x.apply(&self.q_proj).view([batch, seq, self.num_heads, self.head_dim]).permute([0, 2, 1, 3]);
        let k = x.apply(&self.k_proj).view([batch, seq, self.num_heads, self.head_dim]).permute([0, 2, 1, 3]);
        let v = x.apply(&self.v_proj).view([batch, seq, self.num_heads, self.head_dim]).permute([0, 2, 1, 3]);

        // Scaled dot-product attention
        let mut attn = q.matmul(&k.transpose(-2, -1)) * self.scale;

        // Apply attention mask: padded nodes get -inf
        if let Some(m) = mask {
            // m: [batch, seq] → [batch, 1, 1, seq] for broadcasting
            let mask_expanded = m.unsqueeze(1).unsqueeze(2);
            attn = attn.masked_fill(&mask_expanded.logical_not(), f64::NEG_INFINITY);
        }

        let attn_weights = attn.softmax(-1, Kind::Float);
        let attn_weights = if train {
            attn_weights.dropout(dropout, true)
        } else {
            attn_weights
        };

        let out = attn_weights.matmul(&v);
        let out = out.permute([0, 2, 1, 3]).contiguous().view([batch, seq, -1]);
        out.apply(&self.out_proj)
    }
}

// ── Feed-Forward Network ──

pub struct FeedForward {
    fc1: nn::Linear,
    fc2: nn::Linear,
}

impl FeedForward {
    pub fn new(path: &nn::Path, embed_dim: i64, ff_dim: i64) -> Self {
        Self {
            fc1: nn::linear(path / "fc1", embed_dim, ff_dim, Default::default()),
            fc2: nn::linear(path / "fc2", ff_dim, embed_dim, Default::default()),
        }
    }

    pub fn forward(&self, x: &Tensor, train: bool, dropout: f64) -> Tensor {
        let h = x.apply(&self.fc1).gelu("none");
        let h = if train { h.dropout(dropout, true) } else { h };
        h.apply(&self.fc2)
    }
}

// ── Transformer Layer (Pre-LN) ──

pub struct TransformerLayer {
    attn: MultiHeadAttention,
    ff: FeedForward,
    ln1: nn::LayerNorm,
    ln2: nn::LayerNorm,
}

impl TransformerLayer {
    pub fn new(path: &nn::Path, embed_dim: i64, num_heads: i64, ff_dim: i64) -> Self {
        let ln_config = nn::LayerNormConfig { eps: 1e-5, ..Default::default() };
        Self {
            attn: MultiHeadAttention::new(&(path / "attn"), embed_dim, num_heads),
            ff: FeedForward::new(&(path / "ff"), embed_dim, ff_dim),
            ln1: nn::layer_norm(path / "ln1", vec![embed_dim], ln_config),
            ln2: nn::layer_norm(path / "ln2", vec![embed_dim], ln_config),
        }
    }

    pub fn forward(&self, x: &Tensor, mask: Option<&Tensor>, train: bool, dropout: f64) -> Tensor {
        // Pre-LN + Attention + Residual
        let x_norm = x.apply(&self.ln1);
        let attn_out = self.attn.forward(&x_norm, mask, train, dropout);
        let attn_out = if train { attn_out.dropout(dropout, true) } else { attn_out };
        let x = x + attn_out;

        // Pre-LN + FFN + Residual
        let x_norm = x.apply(&self.ln2);
        let ff_out = self.ff.forward(&x_norm, train, dropout);
        let ff_out = if train { ff_out.dropout(dropout, true) } else { ff_out };
        &x + ff_out
    }
}

// ── Graph Transformer Backbone ──

pub struct GraphTransformer {
    input_proj: nn::Linear,
    layers: Vec<TransformerLayer>,
    final_ln: nn::LayerNorm,
    dropout: f64,
}

impl GraphTransformer {
    pub fn new(
        vs: &nn::VarStore,
        input_dim: i64,
        embed_dim: i64,
        num_layers: usize,
        num_heads: i64,
        ff_dim: i64,
        dropout: f64,
    ) -> Self {
        let root = vs.root();
        let input_proj = nn::linear(&root / "input_proj", input_dim, embed_dim, Default::default());

        let layers = (0..num_layers)
            .map(|i| {
                TransformerLayer::new(
                    &(&root / format!("layer_{i}")),
                    embed_dim,
                    num_heads,
                    ff_dim,
                )
            })
            .collect();

        let ln_config = nn::LayerNormConfig { eps: 1e-5, ..Default::default() };
        let final_ln = nn::layer_norm(&root / "final_ln", vec![embed_dim], ln_config);

        Self { input_proj, layers, final_ln, dropout }
    }

    /// Forward pass.
    /// x: [batch, max_nodes, input_dim]
    /// mask: [batch, max_nodes] bool (true = valid node)
    pub fn forward(&self, x: &Tensor, mask: Option<&Tensor>, train: bool) -> Tensor {
        let mut h = x.apply(&self.input_proj);
        if train {
            h = h.dropout(self.dropout, true);
        }

        for layer in &self.layers {
            h = layer.forward(&h, mask, train, self.dropout);
        }

        h.apply(&self.final_ln)
    }
}

// ── Qwirkle Value Network ──

pub struct QwirkleValueNet {
    transformer: GraphTransformer,
    context_proj: nn::Linear,
    value_fc1: nn::Linear,
    value_fc2: nn::Linear,
}

impl QwirkleValueNet {
    pub fn new(vs: &nn::VarStore) -> Self {
        let root = vs.root();
        let transformer = GraphTransformer::new(
            vs, INPUT_DIM, D_MODEL, N_LAYERS, N_HEADS, D_FF, DROPOUT,
        );
        let context_proj = nn::linear(
            &root / "context_proj",
            D_MODEL + CONTEXT_DIM,
            D_MODEL,
            Default::default(),
        );
        let value_fc1 = nn::linear(&root / "value_fc1", D_MODEL, D_MODEL, Default::default());
        let value_fc2 = nn::linear(&root / "value_fc2", D_MODEL, 1, Default::default());

        Self { transformer, context_proj, value_fc1, value_fc2 }
    }

    /// Evaluate a board state.
    /// node_features: [batch, max_nodes, INPUT_DIM]
    /// mask: [batch, max_nodes] bool
    /// context: [batch, CONTEXT_DIM]
    pub fn forward(
        &self,
        node_features: &Tensor,
        mask: &Tensor,
        context: &Tensor,
        train: bool,
    ) -> Tensor {
        // Transformer backbone
        let h = self.transformer.forward(node_features, Some(mask), train);
        // h: [batch, max_nodes, D_MODEL]

        // Mean-pool over valid (non-padded) nodes
        let mask_f = mask.unsqueeze(-1).to_kind(Kind::Float); // [batch, max_nodes, 1]
        let h_masked = &h * &mask_f;
        let sum = h_masked.sum_dim_intlist(1, false, Kind::Float); // [batch, D_MODEL]
        let count = mask_f.sum_dim_intlist(1, false, Kind::Float).clamp_min(1.0); // [batch, 1]
        let pooled = &sum / &count; // [batch, D_MODEL]

        // Concat context and project
        let combined = Tensor::cat(&[pooled, context.shallow_clone()], 1); // [batch, D_MODEL + CONTEXT_DIM]
        let projected = combined.apply(&self.context_proj).gelu("none");

        // Value head MLP
        let v = projected.apply(&self.value_fc1).gelu("none");
        v.apply(&self.value_fc2) // [batch, 1]
    }
}

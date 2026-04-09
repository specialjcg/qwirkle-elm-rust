//! Load and save model weights using safetensors format.

#![cfg(feature = "neural")]

use std::path::Path;
use tch::nn;

/// Save model weights to a safetensors file.
pub fn save_model(vs: &nn::VarStore, path: &str) -> Result<(), Box<dyn std::error::Error>> {
    vs.save(path)?;
    Ok(())
}

/// Load model weights from a safetensors file.
pub fn load_model(vs: &mut nn::VarStore, path: &str) -> Result<(), Box<dyn std::error::Error>> {
    if Path::new(path).exists() {
        vs.load(path)?;
        Ok(())
    } else {
        Err(format!("Model file not found: {path}").into())
    }
}

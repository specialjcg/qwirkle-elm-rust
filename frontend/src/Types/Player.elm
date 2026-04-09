module Types.Player exposing (Player, playerDecoder)

import Json.Decode as Decode exposing (Decoder)
import Types.Tile exposing (RackTile, rackTileDecoder)


type alias Player =
    { id : Int
    , pseudo : String
    , gamePosition : Int
    , points : Int
    , lastTurnPoints : Int
    , rack : List RackTile
    , isTurn : Bool
    }


playerDecoder : Decoder Player
playerDecoder =
    Decode.map7 Player
        (Decode.field "id" Decode.int)
        (Decode.field "pseudo" Decode.string)
        (Decode.field "game_position" Decode.int)
        (Decode.field "points" Decode.int)
        (Decode.field "last_turn_points" Decode.int)
        (Decode.field "rack" (Decode.list rackTileDecoder))
        (Decode.field "is_turn" Decode.bool)

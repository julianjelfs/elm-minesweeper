module Config exposing (..)


type alias Config =
    { dimensions : Int, initialBombs : Int }


config : Config
config =
    { dimensions = 20
    , initialBombs = 50
    }

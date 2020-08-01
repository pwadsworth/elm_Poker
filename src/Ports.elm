port module Ports exposing (scoresFromPokerJS, sendToPokerScorerJS)

-- outbounds


port sendToPokerScorerJS : List ( List Int, List Int ) -> Cmd msg



--inbounds


port scoresFromPokerJS : (List { score : Int, description : String } -> msg) -> Sub msg

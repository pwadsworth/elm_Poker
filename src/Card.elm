module Card exposing (Card, Deck, Msg(..), Rank(..), Suit(..), cardToStr, orderedDeck, shuffleDeck, toScorerFormat)

import Random
import Random.List


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type Suit
    = Spades
    | Hearts
    | Diamonds
    | Clubs


type alias Card =
    ( Rank, Suit )


type alias Deck =
    List Card


type Msg
    = NewDeck Deck


shuffleDeck : Cmd Msg
shuffleDeck =
    let
        initDeck : Random.Generator Deck
        initDeck =
            Random.List.shuffle orderedDeck
    in
    Random.generate NewDeck initDeck


orderedDeck : Deck
orderedDeck =
    let
        ranks =
            [ Ace
            , Two
            , Three
            , Four
            , Five
            , Six
            , Seven
            , Eight
            , Nine
            , Ten
            , Jack
            , Queen
            , King
            ]

        suits =
            [ Spades
            , Hearts
            , Diamonds
            , Clubs
            ]
    in
    List.concatMap (\r -> List.map (\s -> ( r, s )) suits) ranks


cardToStr : Card -> String
cardToStr card =
    let
        rankToStr rank =
            case rank of
                Ace ->
                    "A"

                Two ->
                    "2"

                Three ->
                    "3"

                Four ->
                    "4"

                Five ->
                    "5"

                Six ->
                    "6"

                Seven ->
                    "7"

                Eight ->
                    "8"

                Nine ->
                    "9"

                Ten ->
                    "10"

                Jack ->
                    "J"

                Queen ->
                    "Q"

                King ->
                    "K"
    in
    case card of
        ( r, Spades ) ->
            rankToStr r ++ "S"

        ( r, Hearts ) ->
            rankToStr r ++ "H"

        ( r, Diamonds ) ->
            rankToStr r ++ "D"

        ( r, Clubs ) ->
            rankToStr r ++ "C"


toScorerFormat : List Card -> ( List Int, List Int )
toScorerFormat cards =
    List.map parseForScorerJS cards
        |> List.unzip



--  ScorerJS Ranks A=14, K=13, Q=12, J=11
--           Suits = "S":1, "C":2, "H":4, "D":8 };


parseForScorerJS : Card -> ( Int, Int )
parseForScorerJS card =
    let
        rankToStr rank =
            case rank of
                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

                Seven ->
                    7

                Eight ->
                    8

                Nine ->
                    9

                Ten ->
                    10

                Jack ->
                    11

                Queen ->
                    12

                King ->
                    13

                Ace ->
                    14
    in
    case card of
        ( r, Spades ) ->
            ( rankToStr r, 1 )

        ( r, Clubs ) ->
            ( rankToStr r, 2 )

        ( r, Hearts ) ->
            ( rankToStr r, 4 )

        ( r, Diamonds ) ->
            ( rankToStr r, 8 )

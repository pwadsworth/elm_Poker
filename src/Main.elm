module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Text
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Card exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAtr
import List.Extra
import Ports exposing (..)
import Random exposing (Generator, Seed)
import String



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type GameStage
    = Ante
    | PreFlop
    | Flop
    | Turn
    | River
    | ShowDown

type PlayerState
    = IsFolded
    | IsAllIn
    | IsBetting
    | IsWaiting
    | IsOut


type PlayerType
    = NPC
    | Human
    | Community


type alias HandValue =
    { score : Int, description : String }


type alias Player =
    { cards : List Card
    , stack : Int
    , state : PlayerState
    , type_ : PlayerType
    , name : String
    , handValue : Maybe HandValue
    }


type alias Model =
    { deck : Card.Deck
    , players : List Player
    , communityCards : List Card
    , gameStage : GameStage
    , pot : Int
    , minBet : Int
    , tempBet : Int
    }


type Msg
    = Shuffle
    | Deal
    | CheckCall
    | Raise Int
    | AllIn
    | Fold
    | TempBet Int
    | CardMsg Card.Msg
    | ReceivedScoresFromJS (List { score : Int, description : String })


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initPlayer =
            { cards = []
            , stack = 1000
            , state = IsWaiting
            , type_ = NPC
            , name = "INIT"
            , handValue = Nothing
            }
    in
    ( { deck = orderedDeck
      , players =
            [ { initPlayer | state = IsBetting, type_ = Human, name = "Player" }
            , { initPlayer | name = "NPC1" }
            , { initPlayer | name = "NPC2" }
            , { initPlayer | name = "NPC3" }
            , { initPlayer | name = "NPC4" }
            ]
      , communityCards = []
      , gameStage = Ante
      , pot = 0
      , minBet = 0
      , tempBet = 0
      }
    , Cmd.map CardMsg shuffleDeck
    )



-- UPDATE


getPlayerHandScore : Model -> Cmd Msg
getPlayerHandScore model =
    let
        pocketCards =
            case List.head model.players of
                Just p ->
                    p.cards

                Nothing ->
                    []

        possibleHands =
            List.append pocketCards model.communityCards
                |> List.Extra.subsequences
                |> List.filter (\x -> List.length x == 5)
                |> List.map toScorerFormat
    in
    sendToPokerScorerJS possibleHands


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            playerTurn msg model


playerTurn : Msg -> Model -> ( Model, Cmd Msg )
playerTurn msg model =
    case msg of
        Shuffle ->
            ( model, Cmd.map CardMsg shuffleDeck )

        CardMsg (Card.NewDeck newDeck) ->
            ( { model | deck = newDeck }, Cmd.none )

        TempBet bet ->
            ( { model | tempBet = bet }, Cmd.none )

        Deal ->
            let
                updatedModel =
                    deal model
            in
            ( updatedModel
            , getPlayerHandScore updatedModel
            )

        AllIn ->
            ( { model
                | players = ifBetting (\p -> { p | stack = 0, state = IsAllIn }) model.players
                , pot =
                    case List.Extra.find isBetting model.players of
                        Just player ->
                            player.stack + model.pot

                        Nothing ->
                            model.pot
              }
            , Cmd.none
            )

        CheckCall ->
            ( raise model 0, Cmd.none )

        Raise bet ->
            ( raise model bet, Cmd.none )

        Fold ->
            ( { model
                | players =
                    ifBetting
                        (\p ->
                            { p
                                | state = IsFolded
                                , cards = []
                                , handValue = Nothing
                            }
                        )
                        model.players
              }
            , Cmd.none
            )

        ReceivedScoresFromJS pScores ->
            ( { model
                | players =
                    ifBetting (\p -> { p | handValue = List.Extra.maximumBy (\a -> a.score) pScores }) model.players
              }
            , Cmd.none
            )


raise : Model -> Int -> Model
raise model pBet =
    let
        seed =
            Random.initialSeed pBet
    in
    { model
        | players =
            List.map
                (\p ->
                    { p
                        | stack =
                            if isHuman p then
                                (p.stack - pBet)
                                    |> clamp 0 9999999999

                            else
                                Random.step (Random.int model.minBet p.stack) seed
                                    |> Tuple.first
                        , state = IsWaiting
                    }
                )
                model.players
        , pot = model.pot + (model.minBet + pBet) |> clamp 0 9999999999
    }


deal : Model -> Model
deal model =
    let
        getPlayersCards : List Player -> Deck -> List Player
        getPlayersCards players cards =
            let
                cardPairs =
                    List.Extra.groupsOf 2 cards
            in
            List.map2 (\p cp -> { p | cards = cp }) players cardPairs
    in
    case model.gameStage of
        Ante ->
            { model
                | players = getPlayersCards model.players model.deck
                , deck = model.deck |> List.drop (List.length model.players * 2)
                , gameStage = PreFlop
            }

        PreFlop ->
            { model
                | deck = List.drop 3 model.deck
                , communityCards = List.take 3 model.deck
                , gameStage = Flop
            }

        Flop ->
            { model
                | deck = List.drop 1 model.deck
                , communityCards =
                    model.communityCards |> List.append (List.take 1 model.deck)
                , gameStage = Turn
            }

        Turn ->
            { model
                | deck = List.drop 1 model.deck
                , communityCards =
                    model.communityCards |> List.append (List.take 1 model.deck)
                , gameStage = River
            }

        _ ->
            model



-- VIEW


tableAttrs =
    [ HtmlAtr.style "background-color" "darkgreen"
    , HtmlAtr.style "text-align" "center"
    , HtmlAtr.style "font-weight" "bold"
    ]


view : Model -> Html Msg
view model =
    Grid.container tableAttrs
        [ CDN.stylesheet
        , Grid.row [ Row.attrs [ HtmlAtr.class "pokerHeader" ] ]
            []
        , Grid.row [ Row.attrs [ HtmlAtr.class "pokerMain" ] ]
            [ Grid.col [ Col.attrs [ HtmlAtr.class "pokerTable" ] ]
                [ Grid.row [ Row.aroundXs ] <|
                    playerAreaTop model
                , Grid.row []
                    (gameArea model)
                , Grid.row [ Row.aroundXs ] <|
                    playerAreaBtm model
                ]
            , Grid.col
                [ Col.attrs [ HtmlAtr.class "pokerControls", Border.left ]
                , Col.xs2
                , Col.textAlign Bootstrap.Text.alignXsRight
                ]
                [ List.Extra.find isHuman model.players
                    |> controlsView model
                ]
            ]
        , Grid.row [ Row.attrs [ HtmlAtr.class "pokerFooter" ] ]
            [ handInfoText model ]
        ]


cardAttrs =
    [ Col.lg
    , Col.textAlign Bootstrap.Text.alignMdCenter
    , Col.attrs [ Spacing.p2 ]
    ]


playerAreaTop : Model -> List (Grid.Column Msg)
playerAreaTop model =
    oddsFrom model.players
        |> List.map
            (\p ->
                Grid.col cardAttrs <|
                    List.append
                        [ div [] [ text p.name ] ]
                        (cardsView p.type_ p.cards model.gameStage)
            )


gameArea : Model -> List (Grid.Column Msg)
gameArea model =
    [ Grid.col [ Col.sm8 ] <|
        cardsView Community model.communityCards model.gameStage
    , Grid.col [ Col.sm4, Col.attrs [ HtmlAtr.style "color" "gold" ] ]
        [ h4 [] [ text "Pot" ], h3 [] [ text <| "$" ++ String.fromInt model.pot ] ]
    ]


playerAreaBtm : Model -> List (Grid.Column Msg)
playerAreaBtm model =
    evensFrom model.players
        |> List.map
            (\p ->
                Grid.col cardAttrs <|
                    List.append
                        (cardsView p.type_ p.cards model.gameStage)
                        [ div [] [ text p.name ] ]
            )


cardsView : PlayerType -> List Card -> GameStage -> List (Html Msg)
cardsView typeOfCards cards gameStage =
    let
        cardsBack =
            List.map (\_ -> cardImg "red_back") cards

        cardsFace =
            List.map cardToStr cards
                |> List.map (\c -> cardImg c)

        cardImg cardStr =
            img [ "../img/" ++ cardStr ++ ".png" |> HtmlAtr.src, HtmlAtr.height 100 ] []
    in
    case cards of
        [] ->
            [ cardImg "empty" ]

        _ ->
            case gameStage of
                ShowDown ->
                    cardsFace

                _ ->
                    case typeOfCards of
                        NPC ->
                            cardsBack

                        _ ->
                            cardsFace


controlsView : Model -> Maybe Player -> Html Msg
controlsView model p =
    case p of
        Just player ->
            let
                comnAttrs =
                    [ Button.small
                    , Button.block
                    , Button.attrs [ Spacing.p1 ]
                    , if player.state == IsBetting then
                        Button.disabled False

                      else
                        Button.disabled True
                    ]
            in
            Grid.container [ HtmlAtr.style "text-align" "center" ]
                [ Grid.row [ Row.attrs [ HtmlAtr.class "controlsHeader", HtmlAtr.style "text-decoration" "underline" ] ]
                    [ Grid.col [] [ text "Elm Poker" ] ]
                , br [] []
                , div [ HtmlAtr.class "controlsButtons" ]
                    [ Button.button (Button.onClick Deal :: Button.primary :: comnAttrs) [ text "Deal" ]
                    , Button.button (Button.onClick Fold :: Button.primary :: comnAttrs) [ text "Fold" ]
                    , Button.button (Button.onClick (Raise 0) :: Button.primary :: comnAttrs) [ text "Call/Check" ]
                    ]
                , br [] []
                , div [] [ betInput model player comnAttrs ]
                , br [] []
                , div [ HtmlAtr.style "color" "white" ] [ text <| "Stack: $" ++ String.fromInt player.stack ]
                , Button.button
                    (Button.onClick (Raise player.stack) :: Button.danger :: comnAttrs)
                    [ text "All In" ]
                ]

        Nothing ->
            div [] [ text "Error: No player found in controlsView " ]


betInput : Model -> Player -> List (Button.Option Msg) -> Html Msg
betInput model player comnAttrs =
    Grid.row []
        [ Grid.col []
            [ div []
                [ Button.button
                    (Button.onClick (Raise <| clamp 0 player.stack model.tempBet)
                        :: Button.success
                        :: comnAttrs
                    )
                    [ text "Bet/Raise" ]
                , Input.number
                    [ Input.onInput (String.toInt >> Maybe.withDefault 0 >> TempBet)
                    , Input.attrs
                        [ HtmlAtr.min <| String.fromInt model.minBet
                        , HtmlAtr.max <| String.fromInt <| player.stack - model.minBet
                        ]
                    , if player.state == IsBetting then
                        Input.disabled False

                      else
                        Input.disabled True
                    ]
                ]
            ]
        ]


handInfoText : Model -> Grid.Column Msg
handInfoText model =
    let
        handDescription =
            case List.head model.players of
                Just p ->
                    case p.handValue of
                        Just h ->
                            h.description

                        Nothing ->
                            " "

                Nothing ->
                    " "
    in
    Grid.col []
        [ text handDescription
        ]


handStrength : Int -> Int
handStrength scr =
    --min/max scores from scorer.JS module
    let
        minScore =
            484402

        maxScore =
            10411194
    in
    100 * (scr - minScore) // maxScore



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    scoresFromPokerJS ReceivedScoresFromJS



-- HELPERS


evensFrom : List a -> List a
evensFrom lst =
    evenOrOddFromList (==) lst


oddsFrom : List a -> List a
oddsFrom lst =
    evenOrOddFromList (/=) lst


evenOrOddFromList : (Int -> Int -> Bool) -> List a -> List a
evenOrOddFromList fn lst =
    -- Returns evens elements from list if provide "(==)", odds if provided "(/=)"
    lst
        |> List.indexedMap
            (\i x ->
                if fn (modBy 2 i) 0 then
                    Just x

                else
                    Nothing
            )
        |> List.filterMap identity


defaultPlayer : Player
defaultPlayer =
    { cards = [], stack = 0, state = IsFolded, type_ = NPC, name = "X", handValue = Nothing }


isHuman : Player -> Bool
isHuman p =
    p.type_ == Human


isBetting : Player -> Bool
isBetting p =
    p.state == IsBetting


isWaiting : Player -> Bool
isWaiting p =
    p.state == IsWaiting


ifBetting : (Player -> Player) -> List Player -> List Player
ifBetting =
    List.Extra.updateIf isBetting

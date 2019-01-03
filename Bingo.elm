module Bingo exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random



-- MODEL


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


initialModel : Model
initialModel =
    { entries = initialEntries
    , gameNumber = 1
    , name = "John"
    }


initialEntries : List Entry
initialEntries =
    [ Entry 2 "Doing Agile" 200 False
    , Entry 1 "Future-Proof" 100 False
    , Entry 4 "Rock-Star Ninja" 400 False
    , Entry 3 "In The Cloud" 300 False
    ]


allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | Sort
    | NewRandom Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        NewGame ->
            ( { model
                | entries = initialEntries
              }
            , generateRandomNumber
            )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = not e.marked }

                    else
                        e
            in
            ( { model | entries = List.map markEntry model.entries }, Cmd.none )

        Sort ->
            ( { model | entries = List.sortBy .points model.entries }, Cmd.none )



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)



-- VIEW


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ toString gameNumber


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
    h2 [ id "info", class "classy" ]
        [ playerInfoText ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang.org" ]
            [ text "Powered by Elm" ]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
    ul [] listOfEntries


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum


viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ], button [ onClick Sort ] [ text "Sort" ] ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]



-- main : Html Msg
-- main =
--     view initialModel


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, generateRandomNumber )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

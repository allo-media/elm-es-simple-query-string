module Main exposing (main)

import Browser
import Elastic exposing (Expr(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser exposing (DeadEnd)


type alias Model =
    { input : String
    , query : Result (List DeadEnd) Expr
    }


init : Model
init =
    { input = "", query = Elastic.parse "" }


type Msg
    = UpdateInput String


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "elm-es-simple-query" ]
        , input
            [ onInput UpdateInput
            , placeholder "Enter a search query"
            , size 100
            , value model.input
            ]
            []
        , case model.query of
            Ok value ->
                div []
                    [ h2 [] [ text "Elm data structure" ]
                    , pre [] [ Debug.toString value |> text ]
                    , hr [] []
                    , h2 [] [ text "Serialized" ]
                    , pre [] [ Elastic.serialize value |> text ]
                    ]

            Err err ->
                pre [] [ Debug.toString err |> text ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput input ->
            { model | input = input, query = Elastic.parse input }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

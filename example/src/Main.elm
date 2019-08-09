module Main exposing (main)

import Browser
import Elastic exposing (Expr(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser exposing (DeadEnd)


type alias Model =
    { query : Result (List DeadEnd) Expr }


init : Model
init =
    { query = Ok (Word "") }


type Msg
    = Parse (Result (List DeadEnd) Expr)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "elm-es-simple-query" ]
        , input
            [ onInput (Parse << Elastic.parse)
            , placeholder "Enter a search query"
            , size 100
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
        Parse result ->
            { model | query = result }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

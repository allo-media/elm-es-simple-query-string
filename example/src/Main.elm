module Main exposing (main)

import Browser
import Elastic as Elastic
import Elastic.Expression exposing (Expr(..))
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
        [ input [ onInput (Parse << Elastic.parseQuery) ] []
        , div []
            [ case model.query of
                Ok value ->
                    div []
                        [ Elastic.serializeExpr value |> text
                        , hr [] []
                        , Debug.toString value |> text
                        ]

                Err err ->
                    Debug.toString err |> text
            ]
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

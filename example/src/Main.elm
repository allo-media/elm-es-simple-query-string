module Main exposing (main)

import Browser
import Elastic as Elastic
import Html exposing (..)



-- Check unicode
-- apostophre, tiret.


type alias Model =
    {}


init : Model
init =
    {}


view : Model -> Html msg
view model =
    let
        parsing =
            Elastic.parseQuery "tomatoes (cheese | pickle)"
    in
    case parsing of
        Ok value ->
            div []
                [ Elastic.serializeExpr value |> text
                , hr [] []
                , Debug.toString value |> text
                ]

        Err err ->
            Debug.toString err |> text


update : msg -> Model -> Model
update _ model =
    model


main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

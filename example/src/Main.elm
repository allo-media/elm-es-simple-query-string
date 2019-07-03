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
            Elastic.parseQuery "((\"personne me\" (rappelle | recontacte | téléphone) -\"cette personne\" -\"la personne\" -\"une personne\")) | (\"personne ne m'a\" (rappelé | recontacté | téléphoné)) | ((\"m'a\" | \"m'ont\" | \"m'avez\") (jamais | pas) (rappelé | recontacté | téléphoné)) | ((\"devait\" | \"deviez\") ((\"me\" | \"m'\") (\"appeler\" | \"rappeler\" | \"recontacter\" | \"téléphoner\")) | (\"demandé à\" | \"allais\" | \"devais\") \"être\" (\"rappelé\" | \"recontacté\") | \"toujours pas eu d'appel\")"
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

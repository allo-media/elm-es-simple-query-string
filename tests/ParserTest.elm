module ParserTest exposing (suite)

import Elastic exposing (parseQuery)
import Elastic.Expression exposing (Expr(..))
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    let
        expect expr err result =
            Result.map (Expect.equal <| expr) result
                |> Result.withDefault (Expect.fail err)
    in
    describe "Elastic"
        [ describe "parseQuery"
            [ test "should return a Word expression" <|
                \_ ->
                    parseQuery "hamburger"
                        |> expect (Word "hamburger") "Error to parse Word expression"
            , test "should return an Exact expression" <|
                \_ ->
                    parseQuery "\"Big mac\""
                        |> expect (Exact "Big mac") "Error to parse Exact expression"
            , test "should return a Prefix expression" <|
                \_ ->
                    parseQuery "Big*"
                        |> expect (Prefix "Big") "Error to parse Prefix expression"
            , describe "should return an Exclude expression"
                [ test "without spaces after delimiter" <|
                    \_ ->
                        parseQuery "-pickle"
                            |> expect (Exclude (Word "pickle")) "Error to parse exlude expression without spaces"
                , test "with spaces after delimiter" <|
                    \_ ->
                        parseQuery "- pickle"
                            |> expect (Exclude (Word "pickle")) "Error to parse exlude expression with spaces"
                ]
            , test "should return an OR expression" <|
                \_ ->
                    parseQuery "tomatoes | cheese"
                        |> expect (Or (Word "tomatoes") (Word "cheese")) "Error to parse Or expression"
            , describe "should return an And expression"
                [ test "with spaces as delimiter" <|
                    \_ ->
                        parseQuery "tomatoes cheese"
                            |> expect (And (Word "tomatoes") (Word "cheese")) "Error to parse And expression with spaces as delimiter"
                , test "with + as delimiter" <|
                    \_ ->
                        parseQuery "tomatoes + cheese"
                            |> expect (And (Word "tomatoes") (Word "cheese")) "Error to parse And expression with + as delimiter"
                ]
            , describe "should return a group of expression"
                [ test "with no spaces between expression" <|
                    \_ ->
                        parseQuery "tomatoes (cheese | pickle)"
                            |> expect (And (Word "tomatoes") (Or (Word "cheese") (Word "pickle"))) "Error to parse a group of expression with no spaces between expression"
                , test "with spaces between expression" <|
                    \_ ->
                        parseQuery "( ( \"demandé à\" | \"allais\" | \"devais\" ) \"être\" )"
                            |> expect (And (Or (Or (Exact "demandé à") (Exact "allais")) (Exact "devais")) (Exact "être")) "Error to parse"
                ]
            , test "should failed if the string is empty" <|
                \_ ->
                    parseQuery ""
                        |> Expect.err
            ]
        ]

module ParserTest exposing (suite)

import Elastic exposing (Expr(..))
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
        [ describe "parse"
            [ test "should return a Word expression" <|
                \_ ->
                    Elastic.parse "hamburger"
                        |> expect (Word "hamburger") "Error to parse Word expression"
            , test "should return an Exact expression" <|
                \_ ->
                    Elastic.parse "\"Big mac\""
                        |> expect (Exact "Big mac") "Error to parse Exact expression"
            , test "should return a Prefix expression" <|
                \_ ->
                    Elastic.parse "Big*"
                        |> expect (Prefix "Big") "Error to parse Prefix expression"
            , describe "should return an Exclude expression"
                [ test "without spaces after delimiter" <|
                    \_ ->
                        Elastic.parse "-pickle"
                            |> expect (Exclude (Word "pickle")) "Error to parse exlude expression without spaces"
                , test "with spaces after delimiter" <|
                    \_ ->
                        Elastic.parse "- pickle"
                            |> expect (Exclude (Word "pickle")) "Error to parse exlude expression with spaces"
                ]
            , test "should return an OR expression" <|
                \_ ->
                    Elastic.parse "tomatoes | cheese"
                        |> expect (Or [ Word "tomatoes", Word "cheese" ]) "Error to parse Or expression"
            , describe "should return an And expression"
                [ test "with spaces as delimiter" <|
                    \_ ->
                        Elastic.parse "tomatoes cheese"
                            |> expect (And [ Word "tomatoes", Word "cheese" ]) "Error to parse And expression with spaces as delimiter"
                , test "with + as delimiter" <|
                    \_ ->
                        Elastic.parse "tomatoes + cheese"
                            |> expect (And [ Word "tomatoes", Word "cheese" ]) "Error to parse And expression with + as delimiter"
                ]
            , describe "should return a group of expression"
                [ test "with no spaces between expression" <|
                    \_ ->
                        Elastic.parse "tomatoes (cheese | pickle)"
                            |> expect (And [ Word "tomatoes", Or [ Word "cheese", Word "pickle" ] ]) "Error to parse a group of expression with no spaces between expression"
                , test "with spaces between expression" <|
                    \_ ->
                        Elastic.parse "( ( \"demandé à\" | \"allais\" | \"devais\" ) \"être\" )"
                            |> expect
                                (And
                                    [ Or [ Exact "demandé à", Exact "allais", Exact "devais" ]
                                    , Exact "être"
                                    ]
                                )
                                "Error to parse"
                ]
            , test "should failed if the string is empty" <|
                \_ ->
                    Elastic.parse ""
                        |> Expect.err
            , test "should failed if we have just )" <|
                \_ ->
                    Elastic.parse "big -mac)"
                        |> Expect.err
            , test "should failed with (hello)bye" <|
                \_ ->
                    Elastic.parse "(hello)bye"
                        |> Expect.err
            , test "should handle ambiguous operator precendence" <|
                \_ ->
                    Elastic.parse "a b | a c"
                        |> Expect.equal (Ok (Or [ And [ Word "a", Word "b" ], And [ Word "a", Word "c" ] ]))
            ]
        ]

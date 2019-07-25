module SerializerTest exposing (suite)

import Elastic exposing (serializeExpr)
import Elastic.Expression exposing (Expr(..))
import Expect exposing (Expectation)
import Test exposing (..)


serializeExprWithExplicitOr : Expr -> String
serializeExprWithExplicitOr expr =
    serializeExpr True expr


serializeExprWithOutExplicitOr : Expr -> String
serializeExprWithOutExplicitOr expr =
    serializeExpr False expr


suite : Test
suite =
    describe "Elastic"
        [ describe "serializeExpr with explicitOr"
            [ test "should serialize an Word expression into string" <|
                \_ ->
                    serializeExprWithExplicitOr (Word "hamburger")
                        |> Expect.equal "hamburger"
            , test "should serialize an Prefix expression into string" <|
                \_ ->
                    serializeExprWithExplicitOr (Prefix "hamburger")
                        |> Expect.equal "hamburger*"
            , test "should serialize an Exclude expression with a Word expression into string" <|
                \_ ->
                    serializeExprWithExplicitOr (Exclude (Word "hamburger"))
                        |> Expect.equal "-hamburger"
            , test "should serialize an Exclude expression with a Prefix expression into string" <|
                \_ ->
                    serializeExprWithExplicitOr (Exclude (Prefix "big"))
                        |> Expect.equal "-big*"
            , test "should serialize an Exclude expression with a group expression into string" <|
                \_ ->
                    serializeExprWithExplicitOr (Exclude (Or (Prefix "big") (Exact "french fries")))
                        |> Expect.equal "-((big*) | (\"french fries\"))"
            , test "should serialize an And Expression into string" <|
                \_ ->
                    serializeExprWithExplicitOr (And (Word "hamburger") (Prefix "cheese"))
                        |> Expect.equal "hamburger cheese*"
            , test "should serialize a Or expression with implicit parenthesis into string" <|
                \_ ->
                    serializeExprWithExplicitOr (Or (Prefix "bread") (Exclude (Word "vegetable")))
                        |> Expect.equal "(bread*) | (-vegetable)"
            , test "should serialize a group expression into a string" <|
                \_ ->
                    serializeExprWithExplicitOr (And (Prefix "big") (Or (Word "potatoes") (Exact "french fries")))
                        |> Expect.equal "big* ((potatoes) | (\"french fries\"))"
            ]
        , describe "serializeExpr without explicitOr"
            [ test "should serialize an Word expression into string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (Word "hamburger")
                        |> Expect.equal "hamburger"
            , test "should serialize an Prefix expression into string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (Prefix "hamburger")
                        |> Expect.equal "hamburger*"
            , test "should serialize an Exclude expression with a Word expression into string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (Exclude (Word "hamburger"))
                        |> Expect.equal "-hamburger"
            , test "should serialize an Exclude expression with a Prefix expression into string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (Exclude (Prefix "big"))
                        |> Expect.equal "-big*"
            , test "should serialize an Exclude expression with a group expression into string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (Exclude (Or (Prefix "big") (Exact "french fries")))
                        |> Expect.equal "-(big* | \"french fries\")"
            , test "should serialize an And Expression into string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (And (Word "hamburger") (Prefix "cheese"))
                        |> Expect.equal "hamburger cheese*"
            , test "should serialize a Or expression into string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (Or (Prefix "bread") (Exclude (Word "vegetable")))
                        |> Expect.equal "bread* | -vegetable"
            , test "should serialize a group expression into a string" <|
                \_ ->
                    serializeExprWithOutExplicitOr (And (Prefix "big") (Or (Word "potatoes") (Exact "french fries")))
                        |> Expect.equal "big* (potatoes | \"french fries\")"
            ]
        ]

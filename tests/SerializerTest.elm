module SerializerTest exposing (suite)

import Elastic exposing (serializeExpr)
import Elastic.Expression exposing (Expr(..))
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Elastic"
        [ describe "serializeExpr"
            [ test "should serialize an Word expression into string" <|
                \_ ->
                    serializeExpr (Word "hamburger")
                        |> Expect.equal "hamburger"
            , test "should serialize an Prefix expression into string" <|
                \_ ->
                    serializeExpr (Prefix "hamburger")
                        |> Expect.equal "hamburger*"
            , test "should serialize an Exclude expression with a Word expression into string" <|
                \_ ->
                    serializeExpr (Exclude (Word "hamburger"))
                        |> Expect.equal "-hamburger"
            , test "should serialize an Exclude expression with a Prefix expression into string" <|
                \_ ->
                    serializeExpr (Exclude (Prefix "big"))
                        |> Expect.equal "-big*"
            , test "should serialize an Exclude expression with a group expression into string" <|
                \_ ->
                    serializeExpr (Exclude (Or (Prefix "big") (Exact "french fries")))
                        |> Expect.equal "-(big*|\"french fries\")"
            , test "should serialize an And Expression into string" <|
                \_ ->
                    serializeExpr (And (Word "hamburger") (Prefix "cheese"))
                        |> Expect.equal "hamburger cheese*"
            , test "should serialize a Or exression into string" <|
                \_ ->
                    serializeExpr (Or (Prefix "bread") (Exclude (Word "vegetable")))
                        |> Expect.equal "bread*|-vegetable"
            , test "should serialize a group expression into a string" <|
                \_ ->
                    serializeExpr (And (Prefix "big") (Or (Word "potatoes") (Exact "french fries")))
                        |> Expect.equal "big* (potatoes|\"french fries\")"
            ]
        ]

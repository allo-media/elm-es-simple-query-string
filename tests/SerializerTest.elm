module SerializerTest exposing (suite)

import Elastic
import Elastic.Expression exposing (Expr(..))
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Elastic"
        [ test "should serialize an Word expression into string" <|
            \_ ->
                Elastic.serialize (Word "hamburger")
                    |> Expect.equal "hamburger"
        , test "should serialize an Prefix expression into string" <|
            \_ ->
                Elastic.serialize (Prefix "hamburger")
                    |> Expect.equal "hamburger*"
        , test "should serialize an Exclude expression with a Word expression into string" <|
            \_ ->
                Elastic.serialize (Exclude (Word "hamburger"))
                    |> Expect.equal "-hamburger"
        , test "should serialize an Exclude expression with a Prefix expression into string" <|
            \_ ->
                Elastic.serialize (Exclude (Prefix "big"))
                    |> Expect.equal "-big*"
        , test "should serialize an Exclude expression with a group expression into string" <|
            \_ ->
                Elastic.serialize (Exclude (Or [ Prefix "big", Exact "french fries" ]))
                    |> Expect.equal "-(big* | \"french fries\")"
        , test "should serialize an And Expression into string" <|
            \_ ->
                Elastic.serialize (And [ Word "hamburger", Prefix "cheese" ])
                    |> Expect.equal "hamburger cheese*"
        , test "should serialize a Or expression with implicit parenthesis into string" <|
            \_ ->
                Elastic.serialize (Or [ Prefix "bread", Exclude (Word "vegetable") ])
                    |> Expect.equal "bread* | -vegetable"
        , test "should serialize a group expression into a string" <|
            \_ ->
                Elastic.serialize (And [ Prefix "big", Or [ Word "potatoes", Exact "french fries" ] ])
                    |> Expect.equal "big* (potatoes | \"french fries\")"
        , test "should serialize a nested AND group expression into a string" <|
            \_ ->
                Elastic.serialize (Or [ Prefix "big", And [ Word "potatoes", Word "ketchup", Word "mayo" ] ])
                    |> Expect.equal "big* | potatoes ketchup mayo"
        , test "should serialize a nested OR group expression into a string" <|
            \_ ->
                Elastic.serialize
                    (And
                        [ Or [ Word "potatoes", Word "fries" ]
                        , And
                            [ Word "onions"
                            , Or [ Word "ketchup", Word "mayo" ]
                            ]
                        ]
                    )
                    |> Expect.equal "(potatoes | fries) onions (ketchup | mayo)"
        ]

module Suite exposing (suite)

import Elastic exposing (Expr(..))
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Elastic"
        [ describe "parse"
            [ Elastic.parse ""
                |> Expect.equal (Ok (And []))
                |> asTest "should parse an empty string"
            , Elastic.parse "(a"
                |> Expect.err
                |> asTest "should fail with an non-terminated starting group"
            , Elastic.parse "z)"
                |> Expect.err
                |> asTest "should fail with an non-terminated ending group"
            , Elastic.parse "()"
                |> Expect.err
                |> asTest "should fail with an empty group"
            , Elastic.parse "a"
                |> Expect.equal (Ok (Word "a"))
                |> asTest "should parse a Word expression"
            , Elastic.parse "\"a a\""
                |> Expect.equal (Ok (Exact "a a"))
                |> asTest "should parse an Exact expression"
            , Elastic.parse "a*"
                |> Expect.equal (Ok (Prefix "a"))
                |> asTest "should parse a Prefix expression"
            , Elastic.parse "a~2"
                |> Expect.equal (Ok (Fuzzy 2 "a"))
                |> asTest "should parse a fuzzy match expression"
            , Elastic.parse "-a~2"
                |> Expect.equal (Ok (Exclude (Fuzzy 2 "a")))
                |> asTest "should parse an excluded fuzzy match expression"
            , Elastic.parse "-a"
                |> Expect.equal (Ok (Exclude (Word "a")))
                |> asTest "should parse an Exclude expression"
            , Elastic.parse "- a"
                |> Expect.equal (Ok (Exclude (Word "a")))
                |> asTest "should parse an Exclude expression with a space in-between"
            , Elastic.parse "a | b"
                |> Expect.equal (Ok (Or [ Word "a", Word "b" ]))
                |> asTest "should parse an OR expression"
            , Elastic.parse "a | b | c"
                |> Expect.equal (Ok (Or [ Word "a", Word "b", Word "c" ]))
                |> asTest "should parse sequential OR expressions"
            , Elastic.parse "a|b"
                |> Expect.equal (Ok (Or [ Word "a", Word "b" ]))
                |> asTest "should parse an OR expression in a compact fashion"
            , Elastic.parse "a b"
                |> Expect.equal (Ok (And [ Word "a", Word "b" ]))
                |> asTest "should parse an AND expression"
            , Elastic.parse "a b c"
                |> Expect.equal (Ok (And [ Word "a", Word "b", Word "c" ]))
                |> asTest "should parse sequential AND expressions"
            , Elastic.parse "a + b"
                |> Expect.equal (Ok (And [ Word "a", Word "b" ]))
                |> asTest "should treat the + character as an AND operator"
            , Elastic.parse "a+b"
                |> Expect.equal (Ok (And [ Word "a", Word "b" ]))
                |> asTest "should treat the + character as an AND operator in a compact fashion"
            , Elastic.parse "a b | c d"
                |> Expect.equal (Ok (Or [ And [ Word "a", Word "b" ], And [ Word "c", Word "d" ] ]))
                |> asTest "should parse grouped expressions"
            , Elastic.parse "a b | c d | e f"
                |> Expect.equal (Ok (Or [ And [ Word "a", Word "b" ], And [ Word "c", Word "d" ], And [ Word "e", Word "f" ] ]))
                |> asTest "should parse sequential grouped expressions"
            , Elastic.parse "a + b | c + d"
                |> Expect.equal (Ok (Or [ And [ Word "a", Word "b" ], And [ Word "c", Word "d" ] ]))
                |> asTest "should parse grouped expressions using + for AND expressions"
            , Elastic.parse "a (b | c)"
                |> Expect.equal (Ok (And [ Word "a", Or [ Word "b", Word "c" ] ]))
                |> asTest "should parse composite AND-OR expressions"
            , Elastic.parse "(a | b c)"
                |> Expect.equal (Ok (Or [ Word "a", And [ Word "b", Word "c" ] ]))
                |> asTest "should parse composite OR-AND expressions"
            , Elastic.parse "  (   a   |    b    c   )  "
                |> Expect.equal (Ok (Or [ Word "a", And [ Word "b", Word "c" ] ]))
                |> asTest "should parse and trim spaces appropriately"
            , Elastic.parse "\n(\na\n|\nb\nc\n)\n"
                |> Expect.equal (Ok (Or [ Word "a", And [ Word "b", Word "c" ] ]))
                |> asTest "should parse and trim newlines appropriately"
            , Elastic.parse "\t(\ta\t|\tb\tc\t)\t"
                |> Expect.equal (Ok (Or [ Word "a", And [ Word "b", Word "c" ] ]))
                |> asTest "should parse and trim tab chars appropriately"
            , Elastic.parse "(a)b"
                |> Expect.err
                |> asTest "should fail when missing spaces around expressions"
            , Elastic.parse "a b | a c"
                |> Expect.equal (Ok (Or [ And [ Word "a", Word "b" ], And [ Word "a", Word "c" ] ]))
                |> asTest "should handle ambiguous operator precedence"
            , Elastic.parse "a (b | c) (d | e f | g) h"
                |> Expect.equal
                    (Ok
                        (And
                            [ Word "a"
                            , Or [ Word "b", Word "c" ]
                            , Or [ Word "d", And [ Word "e", Word "f" ], Word "g" ]
                            , Word "h"
                            ]
                        )
                    )
                |> asTest "should handle deeply nested ambiguous operator precedence"
            ]
        , describe "serialize"
            [ Elastic.serialize (Word "a")
                |> Expect.equal "a"
                |> asTest "should serialize an Word expression into string"
            , Elastic.serialize (Prefix "a")
                |> Expect.equal "a*"
                |> asTest "should serialize an Prefix expression into string"
            , Elastic.serialize (Fuzzy 2 "a")
                |> Expect.equal "a~2"
                |> asTest "should serialize a Fuzzy expression into string"
            , Elastic.serialize (Exclude (Word "a"))
                |> Expect.equal "-a"
                |> asTest "should serialize an Exclude expression with a Word expression into string"
            , Elastic.serialize (Exclude (Prefix "a"))
                |> Expect.equal "-a*"
                |> asTest "should serialize an Exclude expression with a Prefix expression into string"
            , Elastic.serialize (Exclude (Fuzzy 2 "a"))
                |> Expect.equal "-a~2"
                |> asTest "should serialize an Exclude expression with a Fuzzy expression into string"
            , Elastic.serialize (Exclude (Or [ Prefix "a", Exact "b c" ]))
                |> Expect.equal "-(a* | \"b c\")"
                |> asTest "should serialize an Exclude expression with a group expression into string"
            , Elastic.serialize (And [ Word "a", Prefix "b" ])
                |> Expect.equal "a b*"
                |> asTest "should serialize an And Expression into string"
            , Elastic.serialize (Or [ Prefix "a", Exclude (Word "b") ])
                |> Expect.equal "a* | -b"
                |> asTest "should serialize a Or expression with implicit parenthesis into string"
            , Elastic.serialize (And [ Prefix "a", Or [ Word "b", Exact "c d" ] ])
                |> Expect.equal "a* (b | \"c d\")"
                |> asTest "should serialize a group expression into a string"
            , Elastic.serialize (Or [ Prefix "a", And [ Word "b", Word "c", Word "d" ] ])
                |> Expect.equal "a* | (b c d)"
                |> asTest "should serialize a nested AND group expression into a string"
            , Elastic.serialize
                (And
                    [ Or [ Word "a", Word "b" ]
                    , And
                        [ Word "c"
                        , Or [ Word "d", Word "e" ]
                        ]
                    ]
                )
                |> Expect.equal "(a | b) (c (d | e))"
                |> asTest "should serialize a nested OR group expression"
            , Elastic.serialize (Or [ And [ Word "a", Word "b" ], And [ Word "a", Word "c" ] ])
                |> Expect.equal "(a b) | (a c)"
                |> asTest "should handle anbiguous operator precedence"
            , Elastic.serialize
                (And
                    [ Word "a"
                    , Or [ Word "b", Word "c" ]
                    , Or [ Word "d", And [ Word "e", Word "f" ], Word "g" ]
                    , Word "h"
                    ]
                )
                |> Expect.equal "a (b | c) (d | (e f) | g) h"
                |> asTest "should handle deeply nested ambiguous operator precedence"
            ]
        ]



-- Helpers


asTest : String -> Expectation -> Test
asTest label =
    always >> test label

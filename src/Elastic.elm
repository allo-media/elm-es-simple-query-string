module Elastic exposing
    ( Expr(..)
    , parse
    , serialize
    )

{-| Parse and serialize [ElasticSearch](https://www.elastic.co/en) search query strings.

This package allows to parse an
[elastic simple query string](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html#_simple_query_string_syntax)
into an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree), and serialize
string search queries out of it.

**Notes:**

  - `~N` operator is not supported.
  - Serialization will enforce classic boolean operator precedence by using
    parenthesis groups everywhere applicable.

[Demo](https://allo-media.github.io/elm-es-simple-query-string/)

@docs Expr


# Parser

@docs parse


# Serializer

@docs serialize

-}

import Elastic.Parser as Parser exposing (Ast(..))
import Parser exposing (DeadEnd)


{-| An ElasticSearh expression.
-}
type Expr
    = And (List Expr)
    | Exact String
    | Exclude Expr
    | Or (List Expr)
    | Prefix String
    | Word String



-- Parser


{-| Parse an ElasticSearch search query string and convert it into an [`Expr`](#Expr).
-}
parse : String -> Result (List DeadEnd) Expr
parse =
    String.trim >> Parser.parse >> Result.map toExpr



-- Serializer


{-| Serialize an [`Expr`](#Expr) to an ElasticSearch query string.

Note: Operator precedence will be enforced by the use of parenthesis groups
everywhere applicable.

-}
serialize : Expr -> String
serialize expr =
    let
        group expr_ =
            case expr_ of
                And _ ->
                    "(" ++ serialize expr_ ++ ")"

                Or _ ->
                    "(" ++ serialize expr_ ++ ")"

                _ ->
                    serialize expr_
    in
    case expr of
        And children ->
            children |> List.map group |> String.join " "

        Exclude first ->
            "-" ++ group first

        Exact string ->
            "\"" ++ string ++ "\""

        Or children ->
            children |> List.map group |> String.join " | "

        Prefix string ->
            string ++ "*"

        Word string ->
            string



-- Internals


isAnd : Ast -> Maybe ( Ast, Ast )
isAnd ast =
    case ast of
        Parser.And a b ->
            Just ( a, b )

        _ ->
            Nothing


isOr : Ast -> Maybe ( Ast, Ast )
isOr ast =
    case ast of
        Parser.Or a b ->
            Just ( a, b )

        _ ->
            Nothing


join : (Ast -> Maybe ( Ast, Ast )) -> Ast -> Ast -> List Expr -> List Expr
join isOperator first second acc =
    case ( isOperator first, isOperator second ) of
        ( Just ( a, b ), Just ( c, d ) ) ->
            List.concat [ acc, join isOperator a b acc, join isOperator c d acc ]

        ( Just ( a, b ), Nothing ) ->
            List.concat [ acc, join isOperator a b acc, [ toExpr second ] ]

        ( Nothing, Just ( c, d ) ) ->
            List.concat [ acc, [ toExpr first ], join isOperator c d acc ]

        ( Nothing, Nothing ) ->
            [ toExpr first, toExpr second ]


toExpr : Ast -> Expr
toExpr expr =
    case expr of
        Parser.And first second ->
            And (join isAnd first second [])

        Parser.Exact string ->
            Exact string

        Parser.Exclude first ->
            Exclude (toExpr first)

        Parser.Or first second ->
            Or (join isOr first second [])

        Parser.Prefix string ->
            Prefix string

        Parser.Word string ->
            Word string

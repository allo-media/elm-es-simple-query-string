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


{-| Parse an ElasticSearch search query string and convert it into an [`Expr`](#Expr).
-}
parse : String -> Result (List DeadEnd) Expr
parse string =
    if String.trim string == "" then
        Ok (And [])

    else
        string
            |> String.trim
            |> Parser.parse
            |> Result.map toExpr


{-| Serialize an [`Expr`](#Expr) to an ElasticSearch query string.

Note: Operator precedence will be enforced by the use of parenthesis groups
everywhere applicable.

-}
serialize : Expr -> String
serialize expr =
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


group : Expr -> String
group expr_ =
    let
        wrap string =
            "(" ++ string ++ ")"
    in
    case expr_ of
        And _ ->
            wrap (serialize expr_)

        Or _ ->
            wrap (serialize expr_)

        _ ->
            serialize expr_


isAnd : Ast -> Maybe (List Ast)
isAnd ast =
    case ast of
        Parser.And a b ->
            Just [ a, b ]

        _ ->
            Nothing


isOr : Ast -> Maybe (List Ast)
isOr ast =
    case ast of
        Parser.Or a b ->
            Just [ a, b ]

        _ ->
            Nothing


join : (Ast -> Maybe (List Ast)) -> List Expr -> List Ast -> List Expr
join isOperator acc =
    List.map
        (\ast ->
            isOperator ast
                |> Maybe.map (join isOperator acc)
                |> Maybe.withDefault [ toExpr ast ]
        )
        >> List.concat
        >> (++) acc


toExpr : Ast -> Expr
toExpr expr =
    case expr of
        Parser.And first second ->
            And (join isAnd [] [ first, second ])

        Parser.Exact string ->
            Exact string

        Parser.Exclude first ->
            Exclude (toExpr first)

        Parser.Or first second ->
            Or (join isOr [] [ first, second ])

        Parser.Prefix string ->
            Prefix string

        Parser.Word string ->
            Word string

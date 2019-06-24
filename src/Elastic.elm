module Elastic exposing
    ( parseQuery
    , serializeExpr
    )

{-| Elm simple elastic query parser & serializer

This package allow to parse an [elastic simple query string](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html#_simple_query_string_syntax) into an AST.

`~N` tokenizer are not supported.


# Parser

@docs parseQuery


# Serializer

@docs serializeExpr

-}

import Elastic.Expression exposing (Expr)
import Elastic.Parser exposing (parse)
import Elastic.Serializer exposing (run)
import Parser exposing (DeadEnd)


{-| parse an elastic simple query string and convert into an AST
-}
parseQuery : String -> Result (List DeadEnd) Expr
parseQuery rawQuery =
    rawQuery
        |> String.trim
        |> parse


{-| serialize an AST into simple query string for elastic search
-}
serializeExpr : Expr -> String
serializeExpr expr =
    run expr

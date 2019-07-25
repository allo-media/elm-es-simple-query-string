module Elastic exposing
    ( parseQuery
    , serializeExpr
    )

{-| Parse and serialize [ElasticSearch](https://www.elastic.co/en) search query strings.

This package allows to parse an [elastic simple query string](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html#_simple_query_string_syntax)
into an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

**Notes:**

  - `~N` operator is not supported.


# Parser

@docs parseQuery


# Serializer

@docs serializeExpr

-}

import Elastic.Expression exposing (Expr)
import Elastic.Parser exposing (parse)
import Elastic.Serializer as Serializer exposing (run)
import Parser exposing (DeadEnd)


{-| Parse an ElasticSearch search query string and convert it into an
[AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
-}
parseQuery : String -> Result (List DeadEnd) Expr
parseQuery rawQuery =
    rawQuery
        |> String.trim
        |> parse


{-| Serialize an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) into
an ElasticSearch search query string.

Options:

  - `explicitOr`: wraps groups with parenthesis to explicit operator precedence.
    This is useful for ambiguous queries like `foo bar | foo baz` which should better
    be treated as `(foo bar) | (foo baz)`.

-}
serializeExpr : { explicitOr : Bool } -> Expr -> String
serializeExpr { explicitOr } expr =
    run (Serializer.Config explicitOr) expr

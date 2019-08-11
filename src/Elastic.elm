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

import Parser exposing (..)
import Set exposing (Set)


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
            |> Parser.run queryExpr


{-| Serialize an [`Expr`](#Expr) to an ElasticSearch query string.

Note: Operator precedence will be enforced by the use of parenthesis groups
everywhere applicable.

-}
serialize : Expr -> String
serialize expr =
    case expr of
        And children ->
            children |> List.map serializeGroup |> String.join " "

        Exclude first ->
            "-" ++ serializeGroup first

        Exact string ->
            "\"" ++ string ++ "\""

        Or children ->
            children |> List.map serializeGroup |> String.join " | "

        Prefix string ->
            string ++ "*"

        Word string ->
            string



-- Parser internals


andExpr : Parser Expr
andExpr =
    excludeExpr
        |> andThen
            (\ast -> loop [ ast ] andExprHelp)


andExprHelp : List Expr -> Parser (Step (List Expr) Expr)
andExprHelp state =
    oneOf
        [ succeed (\ast -> Loop (ast :: state))
            |. backtrackable
                (variable
                    { start = \c -> c == '+' || c == ' '
                    , inner = \c -> c == '+' || c == ' '
                    , reserved = Set.fromList []
                    }
                )
            |= excludeExpr
        , succeed ()
            |> map (state |> toAstList And |> Done |> always)
        ]


exactExpr : Parser Expr
exactExpr =
    succeed Exact
        |. symbol "\""
        |= variable
            { start = \c -> c /= '"'
            , inner = \c -> c /= '"'
            , reserved = Set.fromList []
            }
        |. symbol "\""


excludeExpr : Parser Expr
excludeExpr =
    oneOf
        [ pureExclude
        , groupExp
        ]


groupExp : Parser Expr
groupExp =
    oneOf
        [ exactExpr
        , prefixOrWord
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> orExpr)
            |. spaces
            |. symbol ")"
        ]


isWordChar : Char -> Bool
isWordChar char =
    reservedChar
        |> Set.member char
        |> not


orExpr : Parser Expr
orExpr =
    andExpr
        |> andThen
            (\ast -> loop [ ast ] orExprHelp)


toAstList : (List Expr -> Expr) -> List Expr -> Expr
toAstList toList exprs =
    case exprs of
        [ singleExpr ] ->
            singleExpr

        _ ->
            exprs |> List.reverse |> toList


orExprHelp : List Expr -> Parser (Step (List Expr) Expr)
orExprHelp state =
    oneOf
        [ succeed (\ast -> Loop (ast :: state))
            |. backtrackable spaces
            |. symbol "|"
            |. spaces
            |= andExpr
        , succeed ()
            |> map (state |> toAstList Or |> Done |> always)
        ]


prefixOrWord : Parser Expr
prefixOrWord =
    succeed
        (\word hasSymbol ->
            if hasSymbol then
                Prefix word

            else
                Word word
        )
        |= variable
            { start = isWordChar
            , inner = isWordChar
            , reserved = Set.fromList []
            }
        |= oneOf
            [ map (\_ -> True) (symbol "*")
            , succeed False
            ]


pureExclude : Parser Expr
pureExclude =
    Parser.succeed Exclude
        |. symbol "-"
        |. spaces
        |= groupExp


queryExpr : Parser Expr
queryExpr =
    succeed identity
        |= orExpr
        |. end


reservedChar : Set Char
reservedChar =
    Set.fromList [ '"', '|', '+', '*', '(', ')', ' ' ]



-- Serializer internals


serializeGroup : Expr -> String
serializeGroup expr_ =
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

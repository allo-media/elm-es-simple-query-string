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
  - `\r`, `\n` and `\t` characters will be considered as blank spaces.
  - Serialization will enforce classic boolean operator precedence by using
    parenthesis groups everywhere applicable.

[Demo](https://allo-media.github.io/elm-es-simple-query-string/)

@docs Expr


# Parser

@docs parse


# Serializer

@docs serialize

-}

import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
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


{-| Parse an ElasticSearch search query string and convert it into an
[`Expr`](#Expr).
-}
parse : String -> Result (List DeadEnd) Expr
parse string =
    if String.trim string == "" then
        Ok (And [])

    else
        string
            |> String.trim
            |> String.replace "\n" " "
            |> String.replace "\u{000D}" " "
            |> String.replace "\t" " "
            |> Parser.run parseExpr


{-| Serialize an [`Expr`](#Expr) to an ElasticSearch query string.

**Note:** Operator precedence will be enforced by the use of parenthesis groups
everywhere applicable. That also means this function might act as a formatter as
well as a sanitizer:

    > " a  b  |c  d  " |> parse |> Result.map serialize
    Ok ("(a b) | (c d)") : Result (List Parser.DeadEnd) String

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
        |> Parser.andThen
            (\ast -> Parser.loop [ ast ] andExprHelp)


andExprHelp : List Expr -> Parser (Step (List Expr) Expr)
andExprHelp state =
    Parser.oneOf
        [ Parser.succeed (\ast -> Loop (ast :: state))
            |. Parser.backtrackable
                (Parser.variable
                    { start = \c -> c == '+' || c == ' '
                    , inner = \c -> c == '+' || c == ' '
                    , reserved = Set.fromList []
                    }
                )
            |= excludeExpr
        , Parser.succeed ()
            |> Parser.map (\_ -> state |> toListExpr And |> Done)
        ]


exactExpr : Parser Expr
exactExpr =
    Parser.succeed Exact
        |. Parser.symbol "\""
        |= Parser.variable
            { start = \c -> c /= '"'
            , inner = \c -> c /= '"'
            , reserved = Set.fromList []
            }
        |. Parser.symbol "\""


excludeExpr : Parser Expr
excludeExpr =
    Parser.oneOf
        [ pureExclude
        , groupExpr
        ]


groupExpr : Parser Expr
groupExpr =
    Parser.oneOf
        [ exactExpr
        , prefixOrWord
        , Parser.succeed identity
            |. Parser.symbol "("
            |. Parser.spaces
            |= Parser.lazy (\_ -> orExpr)
            |. Parser.spaces
            |. Parser.symbol ")"
        ]


isWordChar : Char -> Bool
isWordChar char =
    not (Set.member char reservedChar)


orExpr : Parser Expr
orExpr =
    andExpr
        |> Parser.andThen
            (\ast -> Parser.loop [ ast ] orExprHelp)


orExprHelp : List Expr -> Parser (Step (List Expr) Expr)
orExprHelp state =
    Parser.oneOf
        [ Parser.succeed (\ast -> Loop (ast :: state))
            |. Parser.backtrackable Parser.spaces
            |. Parser.symbol "|"
            |. Parser.spaces
            |= andExpr
        , Parser.succeed ()
            |> Parser.map (\_ -> state |> toListExpr Or |> Done)
        ]


parseExpr : Parser Expr
parseExpr =
    Parser.succeed identity
        |= orExpr
        |. Parser.end


prefixOrWord : Parser Expr
prefixOrWord =
    Parser.succeed
        (\word hasSymbol ->
            if hasSymbol then
                Prefix word

            else
                Word word
        )
        |= Parser.variable
            { start = isWordChar
            , inner = isWordChar
            , reserved = Set.fromList []
            }
        |= Parser.oneOf
            [ Parser.map (\_ -> True) (Parser.symbol "*")
            , Parser.succeed False
            ]


pureExclude : Parser Expr
pureExclude =
    Parser.succeed Exclude
        |. Parser.symbol "-"
        |. Parser.spaces
        |= groupExpr


reservedChar : Set Char
reservedChar =
    Set.fromList [ '"', '|', '+', '*', '(', ')', ' ' ]


toListExpr : (List Expr -> Expr) -> List Expr -> Expr
toListExpr operator exprs =
    case exprs of
        [ singleExpr ] ->
            singleExpr

        _ ->
            exprs |> List.reverse |> operator



-- Serializer internals


serializeGroup : Expr -> String
serializeGroup expr =
    let
        wrap string =
            "(" ++ string ++ ")"
    in
    case expr of
        And _ ->
            wrap (serialize expr)

        Or _ ->
            wrap (serialize expr)

        _ ->
            serialize expr

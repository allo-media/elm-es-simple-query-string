module Elastic.Ast exposing (Ast(..), toExpr)

import Elastic.Expression as Expr exposing (Expr)


type Ast
    = And Ast Ast
    | Exact String
    | Exclude Ast
    | Or Ast Ast
    | Prefix String
    | Word String


joinAnd : Ast -> Ast -> List Expr -> List Expr
joinAnd first second acc =
    case ( first, second ) of
        ( And a b, And c d ) ->
            List.concat [ acc, joinAnd a b acc, joinAnd c d acc ]

        ( And a b, y ) ->
            List.concat [ acc, joinAnd a b acc, [ toExpr y ] ]

        ( x, And c d ) ->
            List.concat [ acc, [ toExpr x ], joinAnd c d acc ]

        ( x, y ) ->
            [ toExpr x, toExpr y ]


joinOr : Ast -> Ast -> List Expr -> List Expr
joinOr first second acc =
    case ( first, second ) of
        ( Or a b, Or c d ) ->
            List.concat [ acc, joinOr a b acc, joinOr c d acc ]

        ( Or a b, y ) ->
            List.concat [ acc, joinOr a b acc, [ toExpr y ] ]

        ( x, Or c d ) ->
            List.concat [ acc, [ toExpr x ], joinOr c d acc ]

        ( x, y ) ->
            [ toExpr x, toExpr y ]


toExpr : Ast -> Expr
toExpr expr =
    case expr of
        And first second ->
            Expr.And (joinAnd first second [])

        Exact string ->
            Expr.Exact string

        Exclude first ->
            Expr.Exclude (toExpr first)

        Or first second ->
            Expr.Or (joinOr first second [])

        Prefix string ->
            Expr.Prefix string

        Word string ->
            Expr.Word string

module Elastic.Ast exposing (Ast(..), toExpr)

import Elastic.Expression as Expr exposing (Expr)


type Ast
    = And Ast Ast
    | Exact String
    | Exclude Ast
    | Or Ast Ast
    | Prefix String
    | Word String


isAnd : Ast -> Maybe ( Ast, Ast )
isAnd ast =
    case ast of
        And a b ->
            Just ( a, b )

        _ ->
            Nothing


isOr : Ast -> Maybe ( Ast, Ast )
isOr ast =
    case ast of
        Or a b ->
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
        And first second ->
            Expr.And (join isAnd first second [])

        Exact string ->
            Expr.Exact string

        Exclude first ->
            Expr.Exclude (toExpr first)

        Or first second ->
            Expr.Or (join isOr first second [])

        Prefix string ->
            Expr.Prefix string

        Word string ->
            Expr.Word string

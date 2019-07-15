module Elastic.Expression exposing (Expr(..))

{-| Elastic Expression

Expr can help you to define kind of syntax like And, Or, ....


# Definition

@docs Expr

-}


{-| An `Expr` Type represent all AST's branchs

    type AST
        = And AST AST
        | Or AST AST
        | Exclude AST
        | Exact String
        | Word String
        | Prefix String

In our case, to follow the production rules `AST` will be `Expr`

    type Expr
        = And Expr Expr
        | Exact String
        | Exclude Expr
        | Or Expr Expr
        | Prefix String
        | Word String

-}
type Expr
    = And Expr Expr
    | Exact String
    | Exclude Expr
    | Or Expr Expr
    | Prefix String
    | Word String

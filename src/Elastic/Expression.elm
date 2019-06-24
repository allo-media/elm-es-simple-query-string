module Elastic.Expression exposing (Expr(..))

{-| Expr Type define all AST's branchs
-}


type Expr
    = And Expr Expr
    | Exact String
    | Exclude Expr
    | Or Expr Expr
    | Prefix String
    | Word String

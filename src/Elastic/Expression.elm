module Elastic.Expression exposing (Expr(..))

{-| ElasticSearch query expression.


# Definition

@docs Expr

-}


{-| An ElasticSearh expression.
-}
type Expr
    = And (List Expr)
    | Exact String
    | Exclude Expr
    | Or (List Expr)
    | Prefix String
    | Word String

module Elastic.Serializer exposing (run)

import Elastic.Expression exposing (Expr(..))


group : Expr -> String
group expr =
    "(" ++ run expr ++ ")"


and : Expr -> String
and expr =
    case expr of
        Or _ _ ->
            group expr

        _ ->
            run expr


exclude : Expr -> String
exclude expr =
    case expr of
        And _ _ ->
            group expr

        Or _ _ ->
            group expr

        _ ->
            run expr


run : Expr -> String
run expr =
    case expr of
        And expr_ expr2 ->
            and expr_ ++ " " ++ and expr2

        Exclude expr_ ->
            "-" ++ exclude expr_

        Exact string ->
            "\"" ++ string ++ "\""

        Or expr_ expr2 ->
            run expr_ ++ "|" ++ run expr2

        Prefix string ->
            string ++ "*"

        Word string ->
            string

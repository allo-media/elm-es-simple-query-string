module Elastic.Serializer exposing (run)

import Elastic.Expression exposing (Expr(..))


group : Expr -> String
group expr =
    case expr of
        Or _ ->
            "(" ++ run expr ++ ")"

        _ ->
            run expr


exclude : Expr -> String
exclude expr =
    case expr of
        And _ ->
            group expr

        Or _ ->
            group expr

        _ ->
            run expr


run : Expr -> String
run expr =
    case expr of
        And children ->
            children |> List.map group |> String.join " "

        Exclude first ->
            "-" ++ exclude first

        Exact string ->
            "\"" ++ string ++ "\""

        Or children ->
            children |> List.map group |> String.join " | "

        Prefix string ->
            string ++ "*"

        Word string ->
            string

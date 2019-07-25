module Elastic.Serializer exposing (Config(..), run)

import Elastic.Expression exposing (Expr(..))


type Config
    = Config Bool


group : Config -> Expr -> String
group config expr =
    "(" ++ run config expr ++ ")"


and : Config -> Expr -> String
and config expr =
    case expr of
        Or _ _ ->
            group config expr

        _ ->
            run config expr


exclude : Config -> Expr -> String
exclude config expr =
    case expr of
        And _ _ ->
            group config expr

        Or _ _ ->
            group config expr

        _ ->
            run config expr


run : Config -> Expr -> String
run ((Config explicitOr) as config) expr =
    case expr of
        And expr_ expr2 ->
            and config expr_ ++ " " ++ and config expr2

        Exclude expr_ ->
            "-" ++ exclude config expr_

        Exact string ->
            "\"" ++ string ++ "\""

        Or expr_ expr2 ->
            if explicitOr then
                "(" ++ run config expr_ ++ ")|(" ++ run config expr2 ++ ")"

            else
                run config expr_ ++ "|" ++ run config expr2

        Prefix string ->
            string ++ "*"

        Word string ->
            string

module Elastic.Serializer exposing (Config(..), run)

import Elastic.Expression exposing (Expr(..))


type Config
    = Config { explicitOr : Bool }


group : Config -> Expr -> String
group config expr =
    case expr of
        Or _ _ ->
            "(" ++ run config expr ++ ")"

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
run ((Config { explicitOr }) as config) expr =
    case expr of
        And first second ->
            group config first ++ " " ++ group config second

        Exclude first ->
            "-" ++ exclude config first

        Exact string ->
            "\"" ++ string ++ "\""

        Or first second ->
            group config first ++ " | " ++ group config second

        Prefix string ->
            string ++ "*"

        Word string ->
            string

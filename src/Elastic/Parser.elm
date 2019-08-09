module Elastic.Parser exposing (parse)

import Elastic.Ast as Ast exposing (Ast(..))
import Elastic.Expression exposing (Expr)
import Parser exposing (..)
import Set exposing (Set)


andAst : Parser Ast
andAst =
    excludeAst
        |> andThen
            (\expr -> loop expr andAstHelp)


andAstHelp : Ast -> Parser (Step Ast Ast)
andAstHelp state =
    oneOf
        [ succeed (Loop << And state)
            |. backtrackable
                (variable
                    { start = \c -> c == '+' || c == ' '
                    , inner = \c -> c == '+' || c == ' '
                    , reserved = Set.fromList []
                    }
                )
            |= excludeAst
        , succeed ()
            |> map (\_ -> Done state)
        ]


exactAst : Parser Ast
exactAst =
    succeed Exact
        |. symbol "\""
        |= variable
            { start = \c -> c /= '"'
            , inner = \c -> c /= '"'
            , reserved = Set.fromList []
            }
        |. symbol "\""


excludeAst : Parser Ast
excludeAst =
    oneOf
        [ pureExclude
        , groupExp
        ]


groupExp : Parser Ast
groupExp =
    oneOf
        [ exactAst
        , prefixOrWord
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> orAst)
            |. spaces
            |. symbol ")"
        ]


isWordChar : Char -> Bool
isWordChar char =
    reservedChar
        |> Set.member char
        |> not


orAst : Parser Ast
orAst =
    andAst
        |> andThen
            (\expr -> loop expr orAstHelp)


orAstHelp : Ast -> Parser (Step Ast Ast)
orAstHelp state =
    oneOf
        [ succeed ()
            |. end
            |> map (\_ -> Done state)
        , succeed (Loop << Or state)
            |. backtrackable spaces
            |. symbol "|"
            |. spaces
            |= andAst
        , succeed ()
            |> map (\_ -> Done state)
        ]


parse : String -> Result (List DeadEnd) Expr
parse string =
    run queryAst string
        |> Result.map Ast.toExpr


prefixOrWord : Parser Ast
prefixOrWord =
    succeed
        (\word hasSymbol ->
            if hasSymbol then
                Prefix word

            else
                Word word
        )
        |= variable
            { start = isWordChar
            , inner = isWordChar
            , reserved = Set.fromList []
            }
        |= oneOf
            [ map (\_ -> True) (symbol "*")
            , succeed False
            ]


pureExclude : Parser Ast
pureExclude =
    Parser.succeed Exclude
        |. symbol "-"
        |. spaces
        |= groupExp


queryAst : Parser Ast
queryAst =
    succeed identity
        |= orAst
        |. end


reservedChar : Set Char
reservedChar =
    Set.fromList [ '"', '|', '+', '*', '(', ')', ' ' ]

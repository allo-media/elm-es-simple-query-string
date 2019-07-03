module Elastic.Parser exposing (parse)

import Elastic.Expression exposing (Expr(..))
import Parser exposing (..)
import Set exposing (Set)


andExpr : Parser Expr
andExpr =
    loop (Word "") andExprHelp
        |> andThen checkIfWordIsEmpty


andExprHelp : Expr -> Parser (Step Expr Expr)
andExprHelp state =
    oneOf
        [ succeed Loop
            |= excludeExpr
        , succeed (Loop << And state)
            |. backtrackable
                (variable
                    { start = \c -> c == '+' || c == ' '
                    , inner = \c -> c == '+' || c == ' '
                    , reserved = Set.fromList []
                    }
                )
            |= excludeExpr
        , succeed ()
            |> map (\_ -> Done state)
        ]


checkIfWordIsEmpty : Expr -> Parser Expr
checkIfWordIsEmpty expr =
    if expr == Word "" then
        problem "Empty Word expression."

    else
        succeed expr


exactExpr : Parser Expr
exactExpr =
    succeed Exact
        |. symbol "\""
        |= variable
            { start = \c -> c /= '"'
            , inner = \c -> c /= '"'
            , reserved = Set.fromList []
            }
        |. symbol "\""


excludeExpr : Parser Expr
excludeExpr =
    oneOf
        [ pureExclude
        , groupExp
        ]


groupExp : Parser Expr
groupExp =
    oneOf
        [ exactExpr
        , prefixOrWord
        , succeed identity
            |. symbol "("
            |. spaces
            |= orExpr
            |. spaces
            |. symbol ")"
        ]


isWordChar : Char -> Bool
isWordChar char =
    reservedChar
        |> Set.member char
        |> not


orExpr : Parser Expr
orExpr =
    loop (Word "") orExprHelp
        |> andThen checkIfWordIsEmpty


orExprHelp : Expr -> Parser (Step Expr Expr)
orExprHelp state =
    oneOf
        [ succeed ()
            |. end
            |> map (\_ -> Done state)
        , succeed (Loop << Or state)
            |. backtrackable spaces
            |. symbol "|"
            |. spaces
            |= andExpr
        , succeed Loop
            |= andExpr
        , succeed ()
            |> map (\_ -> Done state)
        ]


parse : String -> Result (List DeadEnd) Expr
parse string =
    run queryExpr string


prefixOrWord : Parser Expr
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


pureExclude : Parser Expr
pureExclude =
    Parser.succeed Exclude
        |. symbol "-"
        |. spaces
        |= groupExp


queryExpr : Parser Expr
queryExpr =
    succeed identity
        |= orExpr
        |. end


reservedChar : Set Char
reservedChar =
    Set.fromList [ '"', '|', '+', '*', '(', ')', ' ' ]

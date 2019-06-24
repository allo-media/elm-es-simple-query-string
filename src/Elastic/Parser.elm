module Elastic.Parser exposing (parse)

import Elastic.Expression exposing (Expr(..))
import Parser as Parser exposing (..)
import Set exposing (Set)


andExpr : Parser Expr
andExpr =
    loop (Word "") andExprHelp
        |> Parser.andThen checkIfWordIsEmpty


andExprHelp : Expr -> Parser (Step Expr Expr)
andExprHelp state =
    oneOf
        [ succeed Loop
            |= excludeExpr
        , succeed (Loop << And state)
            |. Parser.backtrackable
                (variable
                    { start = \c -> c == '+' || c == ' '
                    , inner = \c -> c == '+' || c == ' '
                    , reserved = Set.fromList []
                    }
                )
            |= excludeExpr
        , succeed ()
            |> Parser.map (\_ -> Done state)
        ]


checkIfWordIsEmpty : Expr -> Parser Expr
checkIfWordIsEmpty expr =
    if expr == Word "" then
        Parser.problem "Empty Word expression."

    else
        Parser.succeed expr


exactExpr : Parser Expr
exactExpr =
    Parser.succeed Exact
        |. symbol "\""
        |= variable
            { start = \c -> c /= '"'
            , inner = \c -> c /= '"'
            , reserved = Set.fromList []
            }
        |. symbol "\""


excludeExpr : Parser Expr
excludeExpr =
    Parser.oneOf
        [ pureExclude
        , groupExp
        ]


groupExp : Parser Expr
groupExp =
    Parser.oneOf
        [ exactExpr
        , prefixOrWord
        , Parser.succeed identity
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
        |> Parser.andThen checkIfWordIsEmpty


orExprHelp : Expr -> Parser (Step Expr Expr)
orExprHelp state =
    oneOf
        [ succeed ()
            |. end
            |> Parser.map (\_ -> Done state)
        , succeed (Loop << Or state)
            |. spaces
            |. symbol "|"
            |. spaces
            |= andExpr
        , succeed Loop
            |= andExpr
        , succeed ()
            |> Parser.map (\_ -> Done state)
        ]


parse : String -> Result (List DeadEnd) Expr
parse string =
    run orExpr string


prefixOrWord : Parser Expr
prefixOrWord =
    Parser.succeed
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
            [ Parser.map (\_ -> True) (symbol "*")
            , succeed False
            ]


pureExclude : Parser Expr
pureExclude =
    Parser.succeed Exclude
        |. symbol "-"
        |. spaces
        |= groupExp


reservedChar : Set Char
reservedChar =
    Set.fromList [ '"', '|', '+', '*', '(', ')', ' ' ]

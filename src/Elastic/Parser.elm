module Elastic.Parser exposing (Ast(..), parse)

import Parser exposing (..)
import Set exposing (Set)


type Ast
    = And (List Ast)
    | Exact String
    | Exclude Ast
    | Or (List Ast)
    | Prefix String
    | Word String


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


andAst : Parser Ast
andAst =
    excludeAst
        |> andThen
            (\ast -> loop [ ast ] andAstHelp)


andAstHelp : List Ast -> Parser (Step (List Ast) Ast)
andAstHelp state =
    oneOf
        [ succeed (\ast -> Loop (ast :: state))
            |. backtrackable
                (variable
                    { start = \c -> c == '+' || c == ' '
                    , inner = \c -> c == '+' || c == ' '
                    , reserved = Set.fromList []
                    }
                )
            |= excludeAst
        , succeed ()
            |> map
                (\_ ->
                    Done
                        (case state of
                            [ x ] ->
                                x

                            _ ->
                                And (List.reverse state)
                        )
                )
        ]


orAst : Parser Ast
orAst =
    andAst |> andThen (\ast -> loop [ ast ] orAstHelp)


orAstHelp : List Ast -> Parser (Step (List Ast) Ast)
orAstHelp state =
    oneOf
        [ succeed (\ast -> Loop (ast :: state))
            |. backtrackable spaces
            |. symbol "|"
            |. spaces
            |= andAst
        , succeed ()
            |> map
                (\_ ->
                    Done
                        (case state of
                            [ x ] ->
                                x

                            _ ->
                                Or (List.reverse state)
                        )
                )
        ]


parse : String -> Result (List DeadEnd) Ast
parse =
    run queryAst


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

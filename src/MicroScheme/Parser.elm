module MicroScheme.Parser exposing (..)

-- (exprParser, parse)

import MicroScheme.Expr as Expr exposing (Expr(..))
import MicroScheme.Frame as Frame exposing (Frame)
import Parser as P exposing ((|.), (|=))
import Set


{-|

    TODO: BUG!
    > (define (isEven n) (= (remainder n 2) 0))
    Parse error: [{ col = 1, problem = ExpectingSymbol ")", row = 2 }]

    > parse symbolTable "(plus 33 44)"
    Ok (L [Sym "plus",Z 33,Z 44])

    > newSymbolTable = addSymbol "x" (Sym "x") symbolTable
    Dict.fromList [("plus",Sym "plus"),("times",Sym "times"),("x",Sym "x")]

    > parse newSymbolTable "(plus x 2)"
    Ok (L [Sym "plus",Sym "x",Z 2])

-}
parse : Frame -> String -> Result (List P.DeadEnd) Expr
parse frame str =
    P.run exprParser str |> Result.map (Frame.resolve frame)


pairParser : P.Parser Expr
pairParser =
    P.succeed Pair
        |. P.symbol "("
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |. P.symbol ")"


exprParser : P.Parser Expr
exprParser =
    P.oneOf
        [ P.backtrackable pairParser
        , lambdaParser
        , defineParser
        , ifParser
        , P.lazy (\_ -> listParser)
        , P.backtrackable intParser
        , floatParser
        , stringParser
        , P.lazy (\_ -> defineParser)
        ]


ifParser : P.Parser Expr
ifParser =
    P.succeed If
        |. P.symbol "(if "
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |. P.symbol ")"


defineParser : P.Parser Expr
defineParser =
    P.succeed Define
        |. P.symbol "(define "
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |. P.symbol ")"


lambdaParser : P.Parser Expr
lambdaParser =
    P.succeed Lambda
        |. P.symbol "(lambda "
        |. P.spaces
        |= P.lazy (\_ -> listParser)
        |. P.spaces
        |= P.lazy (\_ -> exprParser)
        |. P.spaces
        |. P.symbol ")"


listParser : P.Parser Expr
listParser =
    P.succeed L
        |. P.symbol "("
        |= many exprParser
        |. P.symbol ")"


intParser : P.Parser Expr
intParser =
    P.map Z P.int


floatParser : P.Parser Expr
floatParser =
    P.map F P.float


stringParser1 : P.Parser Expr
stringParser1 =
    P.map Str
        (P.variable
            { start = \c -> Char.isAlpha c || List.member c [ '=', '<', '>' ]

            -- not <| List.member c [ '(', ')', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
            , inner = \c -> Char.isAlphaNum c && (c /= ')')
            , reserved = Set.fromList [ "eval", "define", "if", "display" ]
            }
        )


stringParser : P.Parser Expr
stringParser =
    let
        prefix =
            \c -> not <| List.member c [ '(', ')', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

        continue =
            \c -> not <| List.member c [ ' ', ')' ]
    in
    text prefix continue |> P.map Str



-- HELPERS


text : (Char -> Bool) -> (Char -> Bool) -> P.Parser String
text prefix continue =
    P.succeed (\start finish content -> String.slice start finish content)
        |= P.getOffset
        |. P.chompIf (\c -> prefix c)
        |. P.chompWhile (\c -> continue c)
        |= P.getOffset
        |= P.getSource


{-| Apply a parser zero or more times and return a list of the results.
-}
many : P.Parser a -> P.Parser (List a)
many p =
    P.loop [] (manyHelp p)


manyHelp : P.Parser a -> List a -> P.Parser (P.Step (List a) (List a))
manyHelp p vs =
    P.oneOf
        [ P.succeed (\v -> P.Loop (v :: vs))
            |= p
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse vs))
        ]

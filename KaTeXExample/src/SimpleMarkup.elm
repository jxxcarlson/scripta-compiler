module SimpleMarkup exposing (Expr(..), compile, exprListParser, parse, render)

import Html exposing (Html)
import KaTeX
import Parser exposing ((|.), (|=), Parser)


type Expr
    = Text String
    | InlineMath String
    | DisplayMath String


compile : String -> Html msg
compile str =
    case parse str of
        Nothing ->
            Html.text "Oops, parse error"

        Just exprList ->
            Html.div [] (List.map render exprList)


render : Expr -> Html msg
render expr =
    case expr of
        Text str ->
            Html.text str

        InlineMath str ->
            KaTeX.inline str

        DisplayMath str ->
            KaTeX.display 500 str


{-|

    > parse "Pythagoras sez $a^2 + b^2 = c^2$.  Yay!"
    Just [Text ("Pythagoras sez "),InlineMath ("a^2 + b^2 = c^2"),Text (".  Yay!")]

-}
parse : String -> Maybe (List Expr)
parse str =
    case Parser.run exprListParser str of
        Err _ ->
            Nothing

        Ok result ->
            Just result


{-|

    > run exprListParser"Pythagoras sez $a^2 + b^2 = c^2$.  Yay!"
    Ok [Text ("Pythagoras sez "),InlineMath ("a^2 + b^2 = c^2"),Text (".  Yay!")]

-}
exprListParser : Parser (List Expr)
exprListParser =
    many exprParser


exprParser : Parser Expr
exprParser =
    Parser.oneOf [ displayMathParser, inlineMathParser, textParser ]


textParser : Parser Expr
textParser =
    Parser.succeed (\start end src -> Text (String.slice start end src))
        |= Parser.getOffset
        |. Parser.chompIf (\c -> c /= '$')
        |. Parser.chompWhile (\c -> c /= '$')
        |= Parser.getOffset
        |= Parser.getSource


inlineMathParser : Parser Expr
inlineMathParser =
    Parser.succeed (\start end src -> InlineMath (String.slice start (end - 1) src))
        |. Parser.symbol "$"
        |= Parser.getOffset
        |. Parser.chompUntil "$"
        |. Parser.symbol "$"
        |= Parser.getOffset
        |= Parser.getSource


displayMathParser : Parser Expr
displayMathParser =
    Parser.succeed (\start end src -> DisplayMath (String.slice start (end - 2) src))
        |. Parser.symbol "$$"
        |= Parser.getOffset
        |. Parser.chompUntil "$$"
        |. Parser.symbol "$$"
        |= Parser.getOffset
        |= Parser.getSource


{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]

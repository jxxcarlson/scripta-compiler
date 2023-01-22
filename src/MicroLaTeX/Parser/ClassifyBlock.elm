module MicroLaTeX.Parser.ClassifyBlock exposing
    ( Classification(..)
    , LXSpecial(..)
    , classificationString
    , classify
    , match
    )

import Parser exposing ((|.), (|=), Parser)


type Classification
    = CBeginBlock String
    | CEndBlock String
    | CSpecialBlock LXSpecial
    | CMathBlockDelim
    | CVerbatimBlockDelim
    | CPlainText
    | CEmpty


match : Classification -> Classification -> Bool
match c1 c2 =
    case ( c1, c2 ) of
        ( CBeginBlock label1, CEndBlock label2 ) ->
            label1 == label2

        ( CMathBlockDelim, CMathBlockDelim ) ->
            True

        ( CVerbatimBlockDelim, CVerbatimBlockDelim ) ->
            False

        ( CSpecialBlock _, _ ) ->
            True

        _ ->
            False


classificationString : Classification -> String
classificationString classification =
    case classification of
        CBeginBlock name ->
            name

        CEndBlock name ->
            name

        _ ->
            "??"


type LXSpecial
    = LXItem
    | LXNumbered
    | LXPseudoBlock
    | LXOrdinaryBlock String
    | LXVerbatimBlock String


classifierParser : Parser Classification
classifierParser =
    Parser.oneOf
        [ beginBlockParser
        , endBlockParser
        , mathBlockDelimParser
        , verbatimBlockDelimParser
        , ordinaryBlockParser
        , verbatimBlockParser
        , itemParser
        , pseudoBlockParser
        , numberedParser
        ]


classify : String -> Classification
classify str =
    let
        str_ =
            String.trimLeft str
    in
    case Parser.run classifierParser str_ of
        Ok classificationOfLine ->
            classificationOfLine

        Err _ ->
            if str == "" then
                CEmpty

            else
                CPlainText


mathBlockDelimParser : Parser Classification
mathBlockDelimParser =
    (Parser.succeed ()
        |. Parser.symbol "$$"
    )
        |> Parser.map (\_ -> CMathBlockDelim)


verbatimBlockDelimParser : Parser Classification
verbatimBlockDelimParser =
    (Parser.succeed ()
        |. Parser.symbol "```"
    )
        |> Parser.map (\_ -> CVerbatimBlockDelim)


beginBlockParser : Parser Classification
beginBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\begin{"
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map CBeginBlock


itemParser : Parser Classification
itemParser =
    Parser.succeed (CSpecialBlock LXItem)
        |. Parser.symbol "\\item"


pseudoBlockParser : Parser Classification
pseudoBlockParser =
    Parser.succeed (CSpecialBlock LXItem)
        |. Parser.oneOf
            [ Parser.symbol "\\section"
            , Parser.symbol "\\subsection"
            , Parser.symbol "\\subsubsection"
            , Parser.symbol "\\image"
            , Parser.symbol "\\title"
            , Parser.symbol "\\contents"
            , Parser.symbol "\\setcounter"
            ]


numberedParser : Parser Classification
numberedParser =
    Parser.succeed (CSpecialBlock LXNumbered)
        |. Parser.symbol "\\numbered"


ordinaryBlockParser : Parser Classification
ordinaryBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "| "
        |= Parser.getOffset
        |. Parser.chompUntilEndOr " "
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> CSpecialBlock (LXOrdinaryBlock s))


verbatimBlockParser : Parser Classification
verbatimBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "|| "
        |= Parser.getOffset
        |. Parser.chompUntilEndOr " "
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> CSpecialBlock (LXVerbatimBlock s))


endBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\end{"
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map CEndBlock

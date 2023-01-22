module Render.Tabular exposing (render)

import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Element exposing (Element)
import List.Extra
import MicroLaTeX.Parser.Expression
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Render.Elm
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Utility
import Utility


render : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
render count acc settings ((ExpressionBlock { lineNumber, args }) as block) =
    let
        formatString : List String
        formatString =
            String.words (List.head args |> Maybe.withDefault "")

        formatList : List (Element.Attribute msg)
        formatList =
            List.map (\c -> Dict.get c formatDict |> Maybe.withDefault Element.centerX) formatString

        lines =
            Render.Utility.getVerbatimContent block |> String.split "\\\\"

        cellsAsStrings_ : List (List String)
        cellsAsStrings_ =
            List.map (String.split "&") lines
                |> List.map (List.map String.trim)

        effectiveFontWidth_ =
            9.0

        maxRowSize : Maybe Int
        maxRowSize =
            List.map List.length cellsAsStrings_ |> List.maximum

        cellsAsStrings =
            List.filter (\row_ -> Just (List.length row_) == maxRowSize) cellsAsStrings_

        columnWidths : List Int
        columnWidths =
            List.map (List.map textWidth) cellsAsStrings
                |> List.Extra.transpose
                |> List.map (\column -> List.maximum column |> Maybe.withDefault 1)
                |> List.map ((\w -> effectiveFontWidth_ * w) >> round)

        fix colWidths fmtList =
            let
                m =
                    List.length colWidths

                n =
                    List.length fmtList
            in
            case compare m n of
                LT ->
                    List.repeat m Element.centerX

                EQ ->
                    fmtList

                GT ->
                    List.repeat m Element.centerX

        extendedFormatList =
            List.map2 (\x y -> ( x, y )) columnWidths (fix columnWidths formatList)

        totalWidth =
            List.sum columnWidths

        parsedCells : List (List (List Expr))
        parsedCells =
            List.map (List.map (MicroLaTeX.Parser.Expression.parse 0 >> Tuple.first)) cellsAsStrings

        renderer : Expr -> Element MarkupMsg
        renderer =
            Render.Elm.render count acc settings

        tableCell : ( Int, Element.Attribute MarkupMsg ) -> List (Element MarkupMsg) -> Element MarkupMsg
        tableCell ( colWidth, fmt ) list =
            Element.el [ Element.width (Element.px (colWidth + 18)) ]
                (Element.row [ Element.paddingXY 12 8, fmt ] list)

        renderCell : ( Int, Element.Attribute MarkupMsg ) -> List Expr -> Element MarkupMsg
        renderCell ( colWidth, fmt ) =
            List.map renderer >> tableCell ( colWidth, fmt )

        renderRow : List ( Int, Element.Attribute MarkupMsg ) -> List (List Expr) -> Element MarkupMsg
        renderRow formats cells =
            List.map2 renderCell formats cells |> Element.row []

        renderTable : List ( Int, Element.Attribute MarkupMsg ) -> List (List (List Expr)) -> List (Element MarkupMsg)
        renderTable formats cells =
            let
                f : List (List Expr) -> Element MarkupMsg
                f =
                    renderRow formats
            in
            List.map (renderRow formats) cells
    in
    Element.column
        [ Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        , Render.Utility.sendLineNumberOnClick lineNumber
        , Render.Utility.idAttribute lineNumber
        ]
        (renderTable extendedFormatList parsedCells)


charDict : Dict String Float
charDict =
    Dict.fromList
        [ ( "a", 1.0 )
        , ( "b", 1.0 )
        , ( "c", 1.0 )
        , ( "d", 1.0 )
        , ( "e", 1.0 )
        , ( "f", 1.0 )
        , ( "g", 1.0 )
        , ( "h", 1.0 )
        , ( "i", 1.0 )
        , ( "j", 1.0 )
        , ( "k", 1.0 )
        , ( "l", 1.0 )
        , ( "m", 1.0 )
        , ( "n", 1.0 )
        , ( "o", 1.0 )
        , ( "p", 1.0 )
        , ( "q", 1.0 )
        , ( "r", 1.0 )
        , ( "s", 1.0 )
        , ( "t", 1.0 )
        , ( "u", 1.0 )
        , ( "v", 1.0 )
        , ( "w", 1.0 )
        , ( "x", 1.0 )
        , ( "y", 1.0 )
        , ( "z", 1.0 )
        , ( "A", 2.0 )
        , ( "B", 2.0 )
        , ( "C", 2.0 )
        , ( "D", 2.0 )
        , ( "E", 2.0 )
        , ( "F", 2.0 )
        , ( "G", 2.0 )
        , ( "H", 2.0 )
        , ( "I", 2.0 )
        , ( "J", 2.0 )
        , ( "K", 2.0 )
        , ( "L", 2.0 )
        , ( "M", 2.0 )
        , ( "N", 2.0 )
        , ( "O", 2.0 )
        , ( "P", 2.0 )
        , ( "Q", 2.0 )
        , ( "R", 2.0 )
        , ( "S", 2.0 )
        , ( "T", 2.0 )
        , ( "U", 2.0 )
        , ( "V", 2.0 )
        , ( "W", 2.0 )
        , ( "X", 2.0 )
        , ( "Y", 2.0 )
        , ( "Z", 2.0 )
        , ( "$", 1.0 )
        ]


formatDict : Dict String (Element.Attribute msg)
formatDict =
    Dict.fromList
        [ ( "l", Element.alignLeft )
        , ( "r", Element.alignRight )
        , ( "c", Element.centerX )
        ]


charWidth : String -> Float
charWidth c =
    Dict.get c charDict |> Maybe.withDefault 1.0


compress string =
    string
        ++ " "
        |> Utility.userReplace "\\\\[a-z].*[^a-zA-Z0-9]" (\_ -> "a")
        |> Utility.userReplace "\\[A-Z].*[^a-zA-Z0-9]" (\_ -> "A")
        |> String.trim


textWidth : String -> Float
textWidth str_ =
    let
        -- \\[a-z]*([^a-z])
        str =
            str_ |> String.words |> List.map compress |> String.join " "

        letters =
            String.split "" str
    in
    letters |> List.map charWidth |> List.sum

module Render.Tabular exposing (render)

import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Element exposing (Element)
import List.Extra
import MicroLaTeX.Parser.Expression
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Render.Expression
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Sync
import Render.Utility
import Utility


render : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
render count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
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
            List.map (List.map Render.Utility.textWidth) cellsAsStrings
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
            Render.Expression.render count acc settings

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
        , Render.Sync.rightLeftSyncHelper lineNumber (lineNumber + numberOfLines)
        , Render.Utility.idAttribute lineNumber
        ]
        (renderTable extendedFormatList parsedCells)


formatDict : Dict String (Element.Attribute msg)
formatDict =
    Dict.fromList
        [ ( "l", Element.alignLeft )
        , ( "r", Element.alignRight )
        , ( "c", Element.centerX )
        ]

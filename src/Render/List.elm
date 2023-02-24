module Render.List exposing (desc, item, numbered)

import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import List.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Render.Helper
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Sync
import Render.Utility



-- LISTS


indentationScale =
    15


item : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
item count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    let
        id =
            String.fromInt lineNumber

        level =
            Dict.get id acc.numberedItemDict |> Maybe.map .level |> Maybe.withDefault 0

        label =
            case modBy 3 level of
                0 ->
                    String.fromChar '●'

                1 ->
                    String.fromChar '○'

                _ ->
                    "◊"
    in
    Element.row [ Element.moveRight (indentationScale * level |> toFloat), Element.alignTop, Render.Utility.idAttribute lineNumber, Render.Utility.vspace 0 settings.topMarginForChildren ]
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Element.moveRight 6
            , Element.width (Element.px 24)
            , Render.Utility.leftPadding settings.leftIndentation
            ]
            (Element.text label)
        , Element.paragraph [ Render.Utility.leftPadding settings.leftIndentation, Render.Sync.rightLeftSyncHelper lineNumber numberOfLines ]
            (Render.Helper.renderWithDefault "| item" count acc settings (Parser.Block.getExprs block))
        ]


numbered : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
numbered count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    let
        id =
            String.fromInt lineNumber

        alphabet =
            [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]

        romanNumerals =
            [ "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx", "xi", "xxii", "xxiii", "xxiv", "xxv", "vi" ]

        alpha k =
            List.Extra.getAt (modBy 26 (k - 1)) alphabet |> Maybe.withDefault "a"

        roman k =
            List.Extra.getAt (modBy 26 (k - 1)) romanNumerals |> Maybe.withDefault "i"

        val =
            Dict.get id acc.numberedItemDict

        index_ =
            val |> Maybe.map .index |> Maybe.withDefault 1

        level =
            val |> Maybe.map .level |> Maybe.withDefault 0

        label =
            case modBy 3 level of
                1 ->
                    alpha index_

                2 ->
                    roman index_

                _ ->
                    String.fromInt index_
    in
    Element.row [ Element.moveRight (indentationScale * level |> toFloat), Element.alignTop, Render.Utility.idAttribute lineNumber, Render.Utility.vspace 0 settings.topMarginForChildren ]
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Element.width (Element.px 24)
            , Render.Utility.leftPadding settings.leftRightIndentation
            ]
            (Element.text (label ++ ". "))
        , Element.paragraph [ Render.Utility.leftPadding settings.leftIndentation, Render.Sync.rightLeftSyncHelper lineNumber numberOfLines ]
            (Render.Helper.renderWithDefault "| numbered" count acc settings (Parser.Block.getExprs block))
        ]


desc : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
desc count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    let
        id =
            String.fromInt lineNumber

        label =
            Render.Utility.argString args
    in
    Element.row ([ Element.alignTop, Render.Utility.idAttribute lineNumber, Render.Utility.vspace 0 settings.topMarginForChildren ] ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings)
        [ Element.el [ Font.bold, Element.alignTop, Element.width (Element.px 100) ] (Element.text label)
        , Element.paragraph [ Render.Utility.leftPadding settings.leftIndentation, Render.Sync.rightLeftSyncHelper lineNumber numberOfLines ]
            (Render.Helper.renderWithDefault "| desc" count acc settings (Parser.Block.getExprs block))
        ]

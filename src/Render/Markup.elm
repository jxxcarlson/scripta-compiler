module Render.Markup exposing (getMessages, renderFromAST, renderFromString, render_)

import Compiler.Acc exposing (Accumulator)
import Element exposing (Element)
import Element.Background
import Element.Font as Font
import Markup
import Parser.Block exposing (ExpressionBlock)
import Parser.BlockUtil as BlockUtil
import Parser.Forest exposing (Forest)
import Parser.Settings
import Render.Block
import Render.Color
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Scripta.Language exposing (Language)
import Tree exposing (Tree)


renderTree : Int -> Accumulator -> Settings -> Tree ExpressionBlock -> Element MarkupMsg
renderTree count acc settings tree =
    let
        root =
            Render.Block.render_ count acc settings (Tree.label tree)
    in
    case Tree.children tree of
        [] ->
            Element.paragraph root.format root.content

        children ->
            Element.column root.format
                (root.content ++ List.map (renderTree count acc settings) children)


renderFromString : Language -> Int -> Accumulator -> Settings -> String -> List (Element MarkupMsg)
renderFromString lang count acc settings str =
    str |> Markup.parse lang |> renderFromAST count acc settings


render_ : Accumulator -> Forest ExpressionBlock -> List (Element MarkupMsg)
render_ acc ast =
    renderFromAST 0 acc Render.Settings.defaultSettings ast


renderFromAST : Int -> Accumulator -> Settings -> Forest ExpressionBlock -> List (Element MarkupMsg)
renderFromAST count accumulator settings ast =
    ast
        |> List.map (renderTree count accumulator settings)


{-| Transform a tree of expression blocks to Element MarkupMsg ("HTML")
-}



--renderTree : Int -> Accumulator -> Settings -> Tree ExpressionBlock -> Element MarkupMsg
--renderTree count accumulator settings tree =
--    let
--        blockName =
--            Parser.Block.getName (Tree.label tree)
--                |> Maybe.withDefault "---"
--
--        root : ExpressionBlock
--        root =
--            Tree.label tree
--    in
--    if List.member blockName Parser.Settings.numberedBlockNames then
--        -- Element.el [ Font.italic ] ((Tree.map (Render.Block.render count accumulator settings) >> unravelFlat) tree)
--        Element.el [ Font.italic ] ((Tree.map (Render.Block.render count accumulator settings) >> unravel) tree)
--
--    else
--        (Tree.map (Render.Block.render count accumulator settings) >> unravel) tree


getMessages : Forest ExpressionBlock -> List String
getMessages syntaxTree =
    syntaxTree
        |> List.map Tree.flatten
        |> List.concat
        |> List.map BlockUtil.getMessages
        |> List.concat


{-| Comment on this! Get better name.
-}
unravelFlat : Tree (Element MarkupMsg) -> Element MarkupMsg
unravelFlat tree =
    let
        children =
            Tree.children tree
    in
    if List.isEmpty children then
        Tree.label tree

    else
        Element.column []
            --  Render.Settings.leftIndentation,
            (Tree.label tree :: List.map unravel children)


unravel : Tree (Element MarkupMsg) -> Element MarkupMsg
unravel tree =
    let
        children =
            Tree.children tree
    in
    if List.isEmpty children then
        Tree.label tree

    else
        Element.column [ Element.Background.color Render.Color.pink ]
            --  Render.Settings.leftIndentation,
            [ Element.el [] (Element.text "BEGIN")
            , Tree.label tree
            , Element.column
                [ Element.paddingEach
                    { top = Render.Settings.topMarginForChildren
                    , left = Render.Settings.leftIndent
                    , right = 0
                    , bottom = 0
                    }
                ]
                (List.map unravel children)
            , Element.el [] (Element.text "END")
            ]

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
renderTree : Int -> Accumulator -> Settings -> Tree ExpressionBlock -> Element MarkupMsg
renderTree count accumulator settings tree =
    let
        blockName =
            Parser.Block.getName (Tree.label tree)
                |> Maybe.withDefault "---"
    in
    if List.member blockName Parser.Settings.numberedBlockNames then
        Element.el [ Font.italic ] ((Tree.map (Render.Block.render count accumulator settings) >> unravel accumulator.language) tree)

    else
        (Tree.map (Render.Block.render count accumulator settings) >> unravel accumulator.language) tree


getMessages : Forest ExpressionBlock -> List String
getMessages syntaxTree =
    syntaxTree
        |> List.map Tree.flatten
        |> List.concat
        |> List.map BlockUtil.getMessages
        |> List.concat


unravel : Language -> Tree (Element MarkupMsg) -> Element MarkupMsg
unravel lang tree =
    let
        children =
            Tree.children tree
    in
    if List.isEmpty children then
        Tree.label tree

    else
        let
            root : Element MarkupMsg
            root =
                Tree.label tree
        in
        Element.column []
            --  Render.Settings.leftIndentation,
            [ root
            , Element.column
                [ Element.paddingEach
                    { top = Render.Settings.topMarginForChildren
                    , left =
                        if lang == Scripta.Language.MicroLaTeXLang then
                            0

                        else
                            Render.Settings.leftIndent
                    , right = 0
                    , bottom = 0
                    }
                ]
                (List.map (unravel lang) children)
            ]

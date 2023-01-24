module Render.MarkupV2 exposing (getMessages, renderFromAST, renderFromString, render_)

import Compiler.Acc exposing (Accumulator)
import Element exposing (Element)
import Element.Background
import Element.Font as Font
import Markup
import Parser.Block exposing (ExpressionBlock)
import Parser.Forest exposing (Forest)
import Parser.Settings
import Parser.Transform as BlockUtil
import Render.BlockV2
import Render.Color
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Scripta.Language exposing (Language)
import Tree exposing (Tree)


renderTree : Int -> Accumulator -> Settings -> Tree ExpressionBlock -> Element MarkupMsg
renderTree count acc settings tree =
    let
        root_ =
            Tree.label tree

        root =
            Render.BlockV2.render_ count acc settings (Tree.label tree)
    in
    renderTree_ count acc settings (root.inherit |> Debug.log "TOP, inherit") tree


renderTree_ : Int -> Accumulator -> Settings -> List (Element.Attribute MarkupMsg) -> Tree ExpressionBlock -> Element MarkupMsg
renderTree_ count acc settings rootAttributes tree =
    let
        root : Render.BlockV2.RenderData MarkupMsg
        root =
            Render.BlockV2.render_ count acc settings (Tree.label tree)
    in
    case Tree.children tree of
        [] ->
            -- Render the leaves
            if List.member root.blockType [ Parser.Block.Paragraph ] then
                Render.BlockV2.renderDatumAsParagraphWithAttributes
                    (rootAttributes ++ [ Element.Background.color Render.Color.pink ])
                    root.body

            else
                Element.column root.outer
                    [ Element.column [ Element.spacing 8, Element.Background.color Render.Color.paleGreen ]
                        [ Render.BlockV2.renderDatumAsRow root.head
                        , Render.BlockV2.renderDatumAsParagraphWithAttributes rootAttributes root.body
                        ]
                    ]

        children ->
            -- Render the root, then the children
            Element.column (root.outer ++ [ Element.Background.color Render.Color.paleBlue ])
                [ Render.BlockV2.renderDatumAsColumn root.head
                , Element.column root.body.format
                    (Render.BlockV2.renderDatumAsColumnWithAttributes rootAttributes root.body
                        :: List.map (renderTree_ count acc settings rootAttributes) children
                    )
                ]


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
getMessages : Forest ExpressionBlock -> List String
getMessages syntaxTree =
    syntaxTree
        |> List.map Tree.flatten
        |> List.concat
        |> List.map BlockUtil.getMessages
        |> List.concat

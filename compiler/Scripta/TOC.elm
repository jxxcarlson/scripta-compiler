module Scripta.TOC exposing (view)

import Compiler.ASTTools
import Compiler.Acc exposing (Accumulator)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Events as Events
import Element.Font as Font
import List.Extra
import Parser.Block exposing (ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Parser.Forest exposing (Forest)
import Render.Block
import Render.Elm
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings
import Render.Utility
import Tree


view : Int -> Accumulator -> Render.Settings.Settings -> Forest ExpressionBlock -> Element Render.Msg.MarkupMsg
view counter acc _ ast =
    case ast |> List.map Tree.flatten |> List.concat |> Compiler.ASTTools.filterBlocksOnName "contents" of
        [] ->
            Element.column [ Element.spacing 8, Element.paddingEach { left = 0, right = 0, top = 0, bottom = 0 } ]
                (prepareFrontMatter counter acc Render.Settings.defaultSettings ast)

        _ ->
            Element.column [ Element.spacing 8, Element.paddingEach { left = 0, right = 0, top = 0, bottom = 0 } ]
                (prepareTOC counter acc Render.Settings.defaultSettings ast)


viewTocItem : Int -> Accumulator -> Render.Settings.Settings -> ExpressionBlock -> Element MarkupMsg
viewTocItem count acc settings (ExpressionBlock { args, content, lineNumber }) =
    case content of
        Left _ ->
            Element.none

        Right exprs ->
            let
                id =
                    String.fromInt lineNumber

                sectionNumber =
                    case List.Extra.getAt 1 args of
                        Just "-" ->
                            Element.none

                        Just s ->
                            Element.el [] (Element.text (s ++ ". "))

                        _ ->
                            Element.none

                label : Element MarkupMsg
                label =
                    Element.paragraph [ tocIndent args ] (sectionNumber :: List.map (Render.Elm.render count acc settings) exprs)
            in
            Element.el [ Events.onClick (SelectId id) ]
                (Element.link [ Font.color (Element.rgb 0 0 0.8) ] { url = Render.Utility.internalLink id, label = label })


prepareTOC : Int -> Accumulator -> Render.Settings.Settings -> Forest ExpressionBlock -> List (Element MarkupMsg)
prepareTOC count acc settings ast =
    let
        rawToc : List ExpressionBlock
        rawToc =
            Compiler.ASTTools.tableOfContents ast

        toc =
            Element.el [ Font.bold, Font.size 18 ] (Element.text "Contents")
                :: (rawToc |> List.map (viewTocItem count acc settings))

        headings =
            getHeadings ast

        banner : Element MarkupMsg
        banner =
            Compiler.ASTTools.banner ast
                |> Maybe.map (Parser.Block.setName "banner_")
                |> Maybe.map (Render.Block.render count acc settings)
                |> Maybe.withDefault Element.none

        titleSize =
            Font.size (round Render.Settings.maxHeadingFontSize)

        subtitleSize =
            Font.size (round (0.7 * Render.Settings.maxHeadingFontSize))

        idAttr =
            Render.Utility.elementAttribute "id" "title"

        title =
            headings.title
                |> (List.map (Render.Elm.render count acc settings) >> Element.paragraph [ titleSize, idAttr ])

        subtitle =
            headings.subtitle
                |> (List.map (Render.Elm.render count acc settings) >> Element.paragraph [ subtitleSize, Font.color (Element.rgb 0.4 0.4 0.4) ])

        spaceBelow k =
            Element.el [ Element.paddingEach { bottom = k, top = 0, left = 0, right = 0 } ] (Element.text " ")
    in
    if List.length rawToc < 2 then
        banner :: title :: subtitle :: []

    else
        banner :: title :: subtitle :: spaceBelow 8 :: toc


prepareFrontMatter : Int -> Accumulator -> Render.Settings.Settings -> Forest ExpressionBlock -> List (Element MarkupMsg)
prepareFrontMatter count acc settings ast =
    let
        headings =
            getHeadings ast

        titleSize =
            Font.size (round Render.Settings.maxHeadingFontSize)

        subtitleSize =
            Font.size (round (0.7 * Render.Settings.maxHeadingFontSize))

        idAttr =
            Render.Utility.elementAttribute "id" "title"

        title =
            headings.title
                |> (List.map (Render.Elm.render count acc settings) >> Element.paragraph [ titleSize, idAttr ])

        subtitle =
            headings.subtitle
                |> (List.map (Render.Elm.render count acc settings) >> Element.paragraph [ subtitleSize, Font.color (Element.rgb 0.4 0.4 0.4) ])
    in
    title :: subtitle :: []


tocIndent args =
    Element.paddingEach { left = tocIndentAux args, right = 0, top = 0, bottom = 0 }


tocIndentAux args =
    case List.head args of
        Nothing ->
            0

        Just str ->
            String.toInt str |> Maybe.withDefault 0 |> (\x -> 12 * (x - 1))


getHeadings : Forest ExpressionBlock -> { title : List Expr, subtitle : List Expr }
getHeadings ast =
    let
        flattened =
            List.map Tree.flatten ast |> List.concat

        title : List Expr
        title =
            flattened
                |> Compiler.ASTTools.filterBlocksOnName "title"
                |> List.map Parser.Block.getContent
                |> List.concat

        --data
        --    |> List.filter (\item -> item.blockType == OrdinaryBlock [ "title" ])
        --    |> List.head
        --    |> Maybe.map .content
        subtitle : List Expr
        subtitle =
            flattened
                |> Compiler.ASTTools.filterBlocksOnName "subtitle"
                |> List.map Parser.Block.getContent
                |> List.concat
    in
    { title = title, subtitle = subtitle }

module Scripta.API exposing
    ( DisplaySettings
    , EditRecord
    , render
    , makeSettings
    , init
    , update
    )


import Render.Msg
import Parser.Block exposing (ExpressionBlock)
import Parser.Forest exposing (Forest)
import Compiler.Transform
import Compiler.DifferentialParser
import L0.Parser.Expression
import MicroLaTeX.Parser.Expression
import Parser.Block exposing (ExpressionBlock)
import Parser.BlockUtil
import Parser.Expr exposing (Expr(..))
import Parser.Forest exposing (Forest)
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Parser.Tree
import Render.Markup
import Compiler.Acc
import Compiler.AbstractDifferentialParser
import Scripta.TOC
import Tree
import Render.Settings
import Tree
import XMarkdown.Expression
import Compiler.DifferentialParser
import Element exposing(..)
import Dict exposing(Dict)
import Scripta.Language exposing(Language(..))
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))

init : Dict String String -> Language -> String -> Compiler.DifferentialParser.EditRecord
init = Compiler.DifferentialParser.init

update : EditRecord -> String -> EditRecord
update = Compiler.DifferentialParser.update


type alias EditRecord =
    Compiler.AbstractDifferentialParser.EditRecord (Tree.Tree PrimitiveBlock) (Tree.Tree ExpressionBlock) Compiler.Acc.Accumulator


type alias DisplaySettings =
    { windowWidth : Int
    , counter : Int
    , selectedId : String
    , selectedSlug : Maybe String
    , scale : Float
    }

-- VIEW

makeSettings : String -> Maybe String -> Float -> Int -> Render.Settings.Settings
makeSettings  id selectedSlug scale width =
   { width = round (scale * toFloat width)
   , titleSize = 30
   , paragraphSpacing = 28
   , showTOC = True
   , showErrorMessages = False
   , selectedId = id
   , selectedSlug = selectedSlug
   , backgroundColor = Element.rgb 1 1 1
   , titlePrefix = ""
   , isStandaloneDocument = False
   }

renderSettings : DisplaySettings -> Render.Settings.Settings
renderSettings ds =
    Render.Settings.makeSettings ds.selectedId ds.selectedSlug ds.scale ds.windowWidth


type alias Config = { titleSize: Int}

config = {titleSize = 18}



render : DisplaySettings -> Compiler.DifferentialParser.EditRecord -> List (Element Render.Msg.MarkupMsg)
render displaySettings editRecord =
  let
    settings = renderSettings displaySettings
  in
  (Scripta.TOC.view displaySettings.counter  editRecord.accumulator (renderSettings displaySettings) editRecord.parsed ) ::  (renderBody displaySettings.counter settings editRecord)


renderBody : Int ->  Render.Settings.Settings -> Compiler.DifferentialParser.EditRecord -> List (Element Render.Msg.MarkupMsg)
renderBody count settings editRecord  = Render.Markup.renderFromAST  count editRecord.accumulator settings (body editRecord)



-- PARSER INTERFACE

{-| -}
parse : Language -> String -> Forest ExpressionBlock
parse lang sourceText =
    let
        parser =
            case lang of
                MicroLaTeXLang ->
                    MicroLaTeX.Parser.Expression.parse

                L0Lang ->
                    L0.Parser.Expression.parseWithMessages

                PlainTextLang ->
                    \_ s -> ( parsePlainText s, [] )

                XMarkdownLang ->
                    \i s -> ( XMarkdown.Expression.parse i s, [] )
    in
    sourceText
        |> toPrimitiveBlockForest lang
        |> Parser.Forest.map (Parser.BlockUtil.toExpressionBlock lang parser)


messagesFromTree : Tree.Tree ExpressionBlock -> List String
messagesFromTree tree =
    List.map Parser.BlockUtil.getMessages (Tree.flatten tree) |> List.concat


messagesFromForest : Forest ExpressionBlock -> List String
messagesFromForest forest =
    List.map messagesFromTree forest |> List.concat


parsePlainText : String -> List Parser.Expr.Expr
parsePlainText str =
    [ Text str { begin = 0, end = 0, index = 0, id = "??" } ]


emptyBlock =
    Parser.PrimitiveBlock.empty


toPrimitiveBlockForest : Language -> String -> Forest PrimitiveBlock
toPrimitiveBlockForest lang str =
    str
        |> String.lines
        |> Parser.PrimitiveBlock.parse lang isVerbatimLine
        |> List.map (Compiler.Transform.transform lang)
        |> Parser.Tree.forestFromBlocks { emptyBlock | indent = -2 } identity identity
        |> Result.withDefault []


isVerbatimLine : String -> Bool
isVerbatimLine str =
    (String.left 2 str == "||")
        || (String.left 3 str == "```")
        || (String.left 16 str == "\\begin{equation}")
        || (String.left 15 str == "\\begin{aligned}")
        || (String.left 15 str == "\\begin{comment}")
        || (String.left 12 str == "\\begin{code}")
        || (String.left 12 str == "\\begin{verbatim}")
        || (String.left 18 str == "\\begin{mathmacros}")
        || (String.left 2 str == "$$")


body : { a | parsed : Forest ExpressionBlock } -> Forest ExpressionBlock
body editRecord =
    editRecord.parsed



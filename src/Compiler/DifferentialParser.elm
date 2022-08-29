module Compiler.DifferentialParser exposing (EditRecord, init, update)

import Compiler.ASTTools
import Compiler.AbstractDifferentialParser
import Compiler.Acc
import Compiler.Transform
import Dict exposing (Dict)
import Either exposing (Either)
import L0.Parser.Classify
import L0.Parser.Expression
import Markup
import MicroLaTeX.Parser.Expression
import Parser.Block exposing (ExpressionBlock(..), ExpressionBlockData)
import Parser.BlockUtil
import Parser.Expr
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Parser.Tree
import Parser.Utility
import Scripta.Language exposing (Language(..))
import Tree exposing (Tree)
import XMarkdown.Expression


type alias EditRecord =
    Compiler.AbstractDifferentialParser.EditRecord PrimitiveBlock ExpressionBlock Compiler.Acc.Accumulator


type alias ExpBlockData =
    { name : Maybe String, args : List String, properties : Dict String String, indent : Int, lineNumber : Int, numberOfLines : Int, id : String, tag : String, blockType : Parser.Block.BlockType, content : Either String (List Parser.Expr.Expr), messages : List String, sourceText : String }


indentation : ExpressionBlock -> Int
indentation (ExpressionBlock data) =
    data.indent


forestFromBlocks : List ExpressionBlock -> List (Tree ExpressionBlock)
forestFromBlocks blocks =
    Parser.Tree.forestFromBlocks Parser.Block.empty indentation blocks |> Result.withDefault []


init : Dict String String -> Language -> String -> EditRecord
init inclusionData lang str =
    let
        initialData =
            makeInitialData inclusionData lang
    in
    Compiler.AbstractDifferentialParser.init (updateFunctions lang) initialData str


default lang =
    { language = lang
    , mathMacros = ""
    , textMacros = ""
    , vectorSize = 4
    }


makeInitialData inclusionData lang =
    let
        keys =
            Dict.keys inclusionData

        macroKeys =
            List.filter (\k -> String.contains "macro" (String.toLower k)) keys
    in
    case List.head macroKeys of
        Nothing ->
            default lang

        Just fileName ->
            case Dict.get fileName inclusionData of
                Nothing ->
                    default lang

                Just macroText_ ->
                    let
                        macroText =
                            macroText_ ++ "\n\n"

                        _ =
                            macroText
                    in
                    { language = lang
                    , mathMacros = Parser.Utility.getKeyedParagraph "|| mathmacros" macroText |> Maybe.withDefault ""
                    , textMacros = Parser.Utility.getKeyedParagraph "|| textmacros" macroText |> Maybe.withDefault ""
                    , vectorSize = 4
                    }


updateFunctions : Language -> Compiler.AbstractDifferentialParser.UpdateFunctions PrimitiveBlock ExpressionBlock Compiler.Acc.Accumulator
updateFunctions lang =
    { chunker = chunker lang -- String -> List chunk
    , chunkEq = chunkEq -- chunk -> chunk -> Bool
    , chunkIndent = .indent
    , chunkParser = parser lang -- : chunk -> parsedChunk
    , forestFromBlocks = forestFromBlocks -- : List parsedChunk -> List (Tree parsedChunk)
    , getMessages = Markup.messagesFromForest -- : List parsedChunk -> List String
    , accMaker = Compiler.Acc.transformAccumulate -- : Scripta.Language.Language -> List parsedChunk -> (acc, List parsedChunk)
    }


getMessages_ : List ExpressionBlock -> List String
getMessages_ blocks =
    List.map Parser.BlockUtil.getMessages blocks |> List.concat


chunkEq : PrimitiveBlock -> PrimitiveBlock -> Bool
chunkEq b1 b2 =
    b1.sourceText == b2.sourceText


update : EditRecord -> String -> EditRecord
update editRecord text =
    Compiler.AbstractDifferentialParser.update (updateFunctions editRecord.lang) editRecord text


chunker : Language -> String -> List PrimitiveBlock
chunker lang str =
    str |> Markup.toPrimitiveBlocks lang |> List.map (Compiler.Transform.transform lang)


parser : Language -> PrimitiveBlock -> ExpressionBlock
parser lang =
    case lang of
        MicroLaTeXLang ->
            Parser.BlockUtil.toExpressionBlock MicroLaTeXLang MicroLaTeX.Parser.Expression.parse

        L0Lang ->
            Parser.BlockUtil.toExpressionBlock L0Lang L0.Parser.Expression.parseWithMessages

        PlainTextLang ->
            Parser.BlockUtil.toExpressionBlock PlainTextLang (\_ s -> ( Markup.parsePlainText s, [] ))

        XMarkdownLang ->
            Parser.BlockUtil.toExpressionBlock XMarkdownLang (\i s -> ( XMarkdown.Expression.parse i s, [] ))


parserOLD : Language -> Tree PrimitiveBlock -> Tree ExpressionBlock
parserOLD lang =
    case lang of
        MicroLaTeXLang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock MicroLaTeXLang MicroLaTeX.Parser.Expression.parse)

        L0Lang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock L0Lang L0.Parser.Expression.parseWithMessages)

        PlainTextLang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock PlainTextLang (\_ s -> ( Markup.parsePlainText s, [] )))

        XMarkdownLang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock XMarkdownLang (\i s -> ( XMarkdown.Expression.parse i s, [] )))

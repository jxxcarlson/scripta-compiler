module Compiler.DifferentialParser exposing
    ( EditRecord
    , chunkLevel
    , chunker
    , diffPostProcess
    , forestFromBlocks
    , init
    , toExprBlock
    , update
    )

import Compiler.AbstractDifferentialParser
import Compiler.Acc
import Compiler.Differ
import Compiler.Transform
import Dict exposing (Dict)
import Either exposing (Either)
import L0.Parser.Expression
import List.Extra
import Markup
import MicroLaTeX.Parser.Expression
import Parser.Block exposing (ExpressionBlock(..), ExpressionBlockData)
import Parser.BlockUtil
import Parser.Expr
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
        initialData : { language : Language, mathMacros : String, textMacros : String, vectorSize : number }
        initialData =
            makeInitialData inclusionData lang
    in
    Compiler.AbstractDifferentialParser.init (updateFunctions lang) initialData (str ++ "\n")


default lang =
    { language = lang
    , mathMacros = ""
    , textMacros = ""
    , vectorSize = 4
    }


makeInitialData : Dict String String -> Language -> { language : Language, mathMacros : String, textMacros : String, vectorSize : number }
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
    { chunker = chunker lang -- String -> List PrimitiveBlock
    , chunkEq = Parser.PrimitiveBlock.eq -- PrimitiveBlock -> PrimitiveBlock -> Bool
    , chunkLevel = chunkLevel -- PrimitiveBlock -> Bool
    , diffPostProcess = diffPostProcess
    , chunkParser = toExprBlock lang --  PrimitiveBlock -> parsedChunk
    , forestFromBlocks = forestFromBlocks -- : List parsedChunk -> List (Tree parsedChunk)
    , getMessages = Markup.messagesFromForest -- : List parsedChunk -> List String
    , accMaker = Compiler.Acc.transformAccumulate -- : Scripta.Language.Language -> Forest parsedChunk -> (acc, Forest parsedChunk)
    }


diffPostProcess : Compiler.Differ.DiffRecord PrimitiveBlock -> Compiler.Differ.DiffRecord PrimitiveBlock
diffPostProcess diffRecord =
    let
        ms : List PrimitiveBlock
        ms =
            diffRecord.middleSegmentInSource

        mt : List PrimitiveBlock
        mt =
            diffRecord.middleSegmentInTarget

        sourceMiddleLineNumberOfBlock : Maybe Int
        sourceMiddleLineNumberOfBlock =
            List.Extra.last ms |> Maybe.map .lineNumber

        sourceMiddleLinesInBlock : Maybe Int
        sourceMiddleLinesInBlock =
            List.Extra.last ms |> Maybe.map (.content >> List.length)

        sourceMiddleLastLine =
            Maybe.map2 (+) sourceMiddleLineNumberOfBlock sourceMiddleLinesInBlock

        targetMiddleLineNumberOfBlock : Maybe Int
        targetMiddleLineNumberOfBlock =
            List.Extra.last mt |> Maybe.map .lineNumber

        targetMiddleLinesInBlock : Maybe Int
        targetMiddleLinesInBlock =
            List.Extra.last mt |> Maybe.map (.content >> List.length)

        targetMiddleLastLine =
            Maybe.map2 (+) targetMiddleLineNumberOfBlock targetMiddleLinesInBlock

        delta =
            Maybe.map2 (-) sourceMiddleLineNumberOfBlock targetMiddleLineNumberOfBlock |> Maybe.withDefault 0

        _ =
            Debug.log "(source, target, delta)" ( sourceMiddleLineNumberOfBlock, targetMiddleLineNumberOfBlock, delta )
    in
    shiftLines delta diffRecord


shiftLines : Int -> Compiler.Differ.DiffRecord PrimitiveBlock -> Compiler.Differ.DiffRecord PrimitiveBlock
shiftLines delta diffRecord =
    let
        newSuffix =
            List.map (shiftLinesInBlock delta) diffRecord.commonSuffix
    in
    { diffRecord | commonSuffix = newSuffix }


shiftLinesInBlock : Int -> PrimitiveBlock -> PrimitiveBlock
shiftLinesInBlock delta block =
    { block | lineNumber = block.lineNumber + delta }


chunkLevel : PrimitiveBlock -> Int
chunkLevel block =
    block.indent
        + (if block.name == Just "item" || block.name == Just "numbered" then
            1

           else
            0
          )


getMessages_ : List ExpressionBlock -> List String
getMessages_ blocks =
    List.map Parser.BlockUtil.getMessages blocks |> List.concat


update : EditRecord -> String -> EditRecord
update editRecord text =
    Compiler.AbstractDifferentialParser.update (updateFunctions editRecord.lang) editRecord (text ++ "\n")


chunker : Language -> String -> List PrimitiveBlock
chunker lang str =
    str |> Markup.toPrimitiveBlocks lang |> List.map (Compiler.Transform.transform lang)


toExprBlock : Language -> PrimitiveBlock -> ExpressionBlock
toExprBlock lang =
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

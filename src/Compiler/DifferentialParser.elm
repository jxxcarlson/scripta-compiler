module Compiler.DifferentialParser exposing (EditRecord, init, update)

import Compiler.ASTTools
import Compiler.AbstractDifferentialParser
import Compiler.Acc
import Dict exposing (Dict)
import L0.Parser.Classify
import L0.Parser.Expression
import Markup
import MicroLaTeX.Parser.Expression
import Parser.Block exposing (ExpressionBlock)
import Parser.BlockUtil
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Scripta.Language exposing (Language(..))
import Tree exposing (Tree)
import XMarkdown.Expression


type alias EditRecord =
    Compiler.AbstractDifferentialParser.EditRecord (Tree PrimitiveBlock) (Tree ExpressionBlock) Compiler.Acc.Accumulator


init : Dict String String -> Language -> String -> EditRecord
init inclusionData lang str =
    let
        chunks : List (Tree PrimitiveBlock)
        chunks =
            chunker lang str

        -- Get the names of the files to include in the main document
        includedFiles : List String
        includedFiles =
            case List.head chunks of
                Nothing ->
                    []

                Just chunk ->
                    let
                        lines =
                            (Tree.label chunk).content
                    in
                    case List.head lines of
                        Nothing ->
                            []

                        Just "|| load-files" ->
                            List.drop 1 lines

                        _ ->
                            []

        -- Prepend macro definitions if any.
        updatedChunks =
            case List.head includedFiles of
                Nothing ->
                    chunks

                Just fileName ->
                    prependContent fileName inclusionData chunks

        -- Tree { content } ->
        ( newAccumulator, parsed ) =
            (List.map (parser lang) >> Compiler.Acc.transformAccumulate lang) updatedChunks
    in
    { lang = lang
    , chunks = chunks
    , parsed = parsed
    , accumulator = newAccumulator
    , messages = Markup.messagesFromForest parsed
    , includedFiles = includedFiles
    }


{-| Prepend a block of tex macros corresponding to the text retrieved from 'dict' with key 'tag'
-}
prependContent : String -> Dict String String -> List (Tree PrimitiveBlock) -> List (Tree PrimitiveBlock)
prependContent tag dict trees =
    List.map Tree.singleton (makeBlocks tag dict) ++ trees


{-| Function makeBlock looks up the text corresponding to 'tag' in 'dict'
and uses it to produce a primitive block of macro definitions.
-}
makeBlocks : String -> Dict String String -> List PrimitiveBlock
makeBlocks tag dict =
    let
        empty =
            []
    in
    case Dict.get tag dict of
        Nothing ->
            []

        Just content ->
            parseL0 (String.lines content)
                |> List.filter (\pb -> List.member pb.name [ Just "mathmacros", Just "textmacros" ])
                |> List.map (\pb -> { pb | content = pb.content |> List.filter (\line_ -> line_ /= "") })


parseL0 : List String -> List PrimitiveBlock
parseL0 lines =
    Parser.PrimitiveBlock.parse L0Lang L0.Parser.Classify.isVerbatimLine lines


update : EditRecord -> String -> EditRecord
update editRecord text =
    Compiler.AbstractDifferentialParser.update (chunker editRecord.lang) (parser editRecord.lang) Markup.messagesFromForest Compiler.Acc.transformAccumulate editRecord text


chunker : Language -> String -> List (Tree PrimitiveBlock)
chunker lang =
    Markup.toPrimitiveBlockForest lang


parser : Language -> Tree PrimitiveBlock -> Tree ExpressionBlock
parser lang =
    case lang of
        MicroLaTeXLang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock MicroLaTeXLang MicroLaTeX.Parser.Expression.parse)

        L0Lang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock L0Lang L0.Parser.Expression.parseWithMessages)

        PlainTextLang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock PlainTextLang (\_ s -> ( Markup.parsePlainText s, [] )))

        XMarkdownLang ->
            Tree.map (Parser.BlockUtil.toExpressionBlock XMarkdownLang (\i s -> ( XMarkdown.Expression.parse i s, [] )))

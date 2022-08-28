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
import Scripta.Language exposing (Language(..))
import Tree exposing (Tree)
import XMarkdown.Expression


type alias EditRecord =
    Compiler.AbstractDifferentialParser.EditRecord (PrimitiveBlock) (ExpressionBlock) Compiler.Acc.Accumulator

type alias ExpBlockData = { name : Maybe String, args : List String, properties : Dict String String, indent : Int, lineNumber : Int, numberOfLines : Int, id : String, tag : String, blockType : Parser.Block.BlockType, content : Either String (List Parser.Expr.Expr), messages : List String, sourceText : String }

unpack : ExpressionBlock -> ExpBlockData
unpack (ExpressionBlock data) = data

pack : ExpBlockData -> ExpressionBlock
pack data = ExpressionBlock data

--foo : ExpressionBlock -> Parser.Tree.Block ExpressionBlockData
foo (ExpressionBlock data) = Parser.Tree.Block data

forestFromBlocks : List ExpressionBlock -> List (Tree ExpressionBlock)
forestFromBlocks blocks =
-- forestFromBlocks : data -> (Block data -> data) -> (data -> Block data) -> List (Block data) -> Result Error (Forest data)
   Parser.Tree.forestFromBlocks Parser.Block.empty unpack pack (List.map unpack blocks) |> Result.withDefault []
  -- Debug.todo "forestFromBlocks"

type alias Blockkk data =
    { data | indent : Int }


forestFromBlocks1 : List ExpBlockData -> List (Tree ExpBlockData)
forestFromBlocks1 blocks =
    Parser.Tree.forestFromBlocks Parser.Block.empty_ identity identity blocks |> Result.withDefault []

init : Dict String String -> Language -> String -> EditRecord
init inclusionData lang str =
    let
        chunks : List (PrimitiveBlock)
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
                            chunk.content
                    in
                    case List.head lines of
                        Nothing ->
                            []

                        Just "|| load-files" ->
                            List.drop 1 lines

                        _ ->
                            []

        -- Prepend macro definitions if any.
        updatedChunks : List PrimitiveBlock
        updatedChunks =
            case List.head includedFiles of
                Nothing ->
                    chunks

                Just fileName ->
                    prependContent fileName inclusionData chunks

        parsed_ = List.map (Markup.primitiveBlockToExpressionBlock lang) chunks
        tree = forestFromBlocks parsed_

        -- Tree { content } ->
        ( newAccumulator, parsed ) =
            (Compiler.Acc.transformAccumulate lang) tree
    in
    { lang = lang
    , chunks = chunks
    , parsed = parsed_
    , tree = tree
    , accumulator = newAccumulator
    , messages = Markup.messagesFromForest parsed
    , includedFiles = includedFiles
    }


{-| Prepend a block of tex macros corresponding to the text retrieved from 'dict' with key 'tag'
-}
prependContent : String -> Dict String String -> List (PrimitiveBlock) -> List ( PrimitiveBlock)
prependContent tag dict blocks =
    (makeBlocks tag dict) ++ blocks


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

updateFunctions : Language -> Compiler.AbstractDifferentialParser.UpdateFunctions PrimitiveBlock ExpressionBlock Compiler.Acc.Accumulator
updateFunctions lang =
    {  chunker = chunker lang -- String -> List chunk
     , chunkEq = chunkEq -- chunk -> chunk -> Bool
     , chunkParser = parser lang -- : chunk -> parsedChunk
     , forestFromBlocks = forestFromBlocks -- : List parsedChunk -> List (Tree parsedChunk)
     , getMessages = Markup.messagesFromForest -- : List parsedChunk -> List String
     , accMaker = Compiler.Acc.transformAccumulate -- : Scripta.Language.Language -> List parsedChunk -> (acc, List parsedChunk)
    }

getMessages_ : List ExpressionBlock -> List String
getMessages_ blocks =
    List.map Parser.BlockUtil.getMessages blocks |> List.concat


chunkEq : PrimitiveBlock -> PrimitiveBlock -> Bool
chunkEq b1 b2 = b1.sourceText == b2.sourceText

-- update : EditRecord -> String -> EditRecord
update : EditRecord -> String -> EditRecord
update editRecord text =
    Compiler.AbstractDifferentialParser.update (updateFunctions editRecord.lang) editRecord text


chunker : Language -> String -> List ( PrimitiveBlock)
chunker lang str =
    str |> Markup.toPrimitiveBlocks lang |> List.map (Compiler.Transform.transform lang)


parser : Language -> PrimitiveBlock -> ExpressionBlock
parser lang =
    case lang of
        MicroLaTeXLang ->
             (Parser.BlockUtil.toExpressionBlock MicroLaTeXLang MicroLaTeX.Parser.Expression.parse)

        L0Lang ->
           (Parser.BlockUtil.toExpressionBlock L0Lang L0.Parser.Expression.parseWithMessages)

        PlainTextLang ->
             (Parser.BlockUtil.toExpressionBlock PlainTextLang (\_ s -> ( Markup.parsePlainText s, [] )))

        XMarkdownLang ->
           (Parser.BlockUtil.toExpressionBlock XMarkdownLang (\i s -> ( XMarkdown.Expression.parse i s, [] )))


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

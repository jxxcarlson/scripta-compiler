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
           {  language  = lang
             , mathMacros = ""
             , textMacros  = ""
             , vectorSize = 4
             }
  in
  Compiler.AbstractDifferentialParser.init (updateFunctions lang) initialData str

--
--init2 : Dict String String -> Language -> String -> EditRecord
--init2 inclusionData lang str =
--    let
--        chunks : List PrimitiveBlock
--        chunks =
--            chunker lang str
--
--
--
--        includedFiles = getIncludedFiles chunks
--
--        macroData = getMacroData includedFiles inclusionData lang
--
--        parsed_ =
--            List.map (Markup.primitiveBlockToExpressionBlock lang) chunks
--
--        tree_ =
--            forestFromBlocks parsed_
--
--        -- Tree { content } ->
--        ( newAccumulator, tree ) =
--            Compiler.Acc.transformAccumulate lang tree_
--    in
--    { lang = lang
--    , chunks = chunks
--    , parsed = parsed_
--    , tree = tree
--    , accumulator = newAccumulator
--    , messages = Markup.messagesFromForest tree
--    , includedFiles = includedFiles
--    }

getMacroData includedFiles inclusionData lang =
    case List.head includedFiles of
            Nothing -> []
            Just macroFileName ->
                makeBlocks macroFileName inclusionData lang

-- A document with files to be loaded has the form
-- || load-files
-- file1
-- file2
-- ..
-- | title
-- Chem 101
-- ...
-- So the below is how we extract those file names from the document
-- Note that the dictionary 'inclusionData : Dict String String'
-- has keys which are file (document) names and whose values
-- are document contents
getIncludedFiles : List PrimitiveBlock -> List String
getIncludedFiles chunks =
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

{-| Function makeBlock looks up the text corresponding to 'tag' in 'dict'
and uses it to produce a primitive block of macro definitions.
-}
makeBlocks : String -> Dict String String -> Language -> List ExpressionBlock
makeBlocks tag dict lang =
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
                |> List.map (Markup.primitiveBlockToExpressionBlock lang)


parseL0 : List String -> List PrimitiveBlock
parseL0 lines =
    Parser.PrimitiveBlock.parse L0Lang L0.Parser.Classify.isVerbatimLine lines


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



-- update : EditRecord -> String -> EditRecord


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

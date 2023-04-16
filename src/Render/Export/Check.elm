module Render.Export.Check exposing (nonStandardBlocks)

import Compiler.ASTTools
import Parser.Block
import Render.Block
import Tree exposing (Tree)


nonStandardBlocks : List (Tree.Tree Parser.Block.ExpressionBlock) -> List String
nonStandardBlocks forest =
    forest |> Compiler.ASTTools.blockNames |> List.filter (\block -> List.member block nonStandardBlockNameList)


nonStandardBlockNameList =
    Render.Block.nonstandardVerbatimBlocks ++ Render.Block.nonStandardOrdinaryBlocks

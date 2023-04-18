module Render.Export.Check exposing (nonExportableBlocks, nonExportableExpressions)

import Compiler.ASTTools
import Parser.Block
import Render.Block
import Render.Expression
import Tree exposing (Tree)


nonExportableBlocks : List (Tree.Tree Parser.Block.ExpressionBlock) -> List String
nonExportableBlocks forest =
    forest |> Compiler.ASTTools.blockNames |> List.filter (\block -> List.member block nonExportableBlockNameList)


nonExportableExpressions : List (Tree.Tree Parser.Block.ExpressionBlock) -> List String
nonExportableExpressions forest =
    forest |> Compiler.ASTTools.expressionNames |> List.filter (\expr -> List.member expr Render.Expression.nonstandardElements)


nonExportableBlockNameList =
    Render.Block.nonExportableVerbatimBlocks ++ Render.Block.nonExportableOrdinaryBlocks

module Compiler.PrimitiveBlock exposing (diff, diffMicroLaTeX, noChange, x1, x2)

import Compiler.Differ
import Compiler.DifferForest
import Compiler.DifferentialParser
import Parser.PrimitiveBlock
import Scripta.Language


noChange : Scripta.Language.Language -> String -> String -> Bool
noChange lang source1 source2 =
    let
        dr =
            diff lang source1 source2
    in
    dr.middleSegmentInTarget == [] && dr.middleSegmentInSource == [] && dr.commonSuffix == []


diffMicroLaTeX : String -> String -> Compiler.Differ.DiffRecord Parser.PrimitiveBlock.PrimitiveBlock
diffMicroLaTeX =
    diff Scripta.Language.MicroLaTeXLang


diff : Scripta.Language.Language -> String -> String -> Compiler.Differ.DiffRecord Parser.PrimitiveBlock.PrimitiveBlock
diff lang =
    diff_ (chunker lang) chunkEq chunkLevel diffPostProcess


diff_ :
    (String -> List chunk)
    -> (chunk -> chunk -> Bool)
    -> (chunk -> Int)
    -> (Compiler.Differ.DiffRecord chunk -> Compiler.Differ.DiffRecord chunk)
    -> String
    -> String
    -> Compiler.Differ.DiffRecord chunk
diff_ chunker_ chunkEq_ chunkLevel_ diffPostProcess_ source1 source2 =
    let
        chunks1 =
            chunker_ source1

        chunks2 =
            chunker_ source2
    in
    Compiler.DifferForest.diff chunkEq_ chunkLevel_ chunks1 chunks2


chunker lang =
    Compiler.DifferentialParser.chunker lang


chunkLevel =
    Compiler.DifferentialParser.chunkLevel


chunkEq =
    Parser.PrimitiveBlock.eq


diffPostProcess =
    Compiler.DifferentialParser.diffPostProcess


x1 =
    """
a1
a2

b1
b2

c1
c2
"""


x2 =
    """
a1
a2

b1
b2
b3

c1
c2
"""

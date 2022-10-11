module DiffTests exposing (..)

import Expect exposing (equal)
import Test exposing (Test, describe, test)
import Compiler.Differ as D exposing(DiffRecord)
import Compiler.DifferEq as DE
import Parser.Utility
import Markup
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Parser.Line exposing (PrimitiveBlockType(..))
import Dict
import Scripta.Language exposing(Language(..))

--- STRINGS

diffS : List String -> List String -> DiffRecord String
diffS = DE.diff (\a b -> a == b) (\a -> String.length (Parser.Utility.getLeadingBlanks a))

diffB : List PrimitiveBlock -> List PrimitiveBlock -> DiffRecord PrimitiveBlock
diffB = DE.diff Parser.PrimitiveBlock.eq (\b -> b.indent)


testB : String -> List PrimitiveBlock -> List PrimitiveBlock -> DiffRecord PrimitiveBlock -> Test
testB label a b c =
     test label <| \_ -> equal (diffB a b) c

testS : String -> List String -> List String -> DiffRecord String -> Test
testS label a b c =
     test label <| \_ -> equal (diffS a b) c

toPrimitiveBlocks =
    Markup.toPrimitiveBlocks L0Lang

suite : Test
suite =
    describe "Compiler.DifferEq.diff"
        [ testS  "middle string is different" x1 y1 d1
        , testS  "initial string is different" x2 y2 d2
        , testS  "final string is different" x3 y3 d3
        , testS  "indented string 3 different, difference should go back to string 1 (root)" x4 y4 d4
        , testS  "indented string 2 different, difference should go forward to string 3 and back to string 1 (root)" x5 y5 d5
        , testS  "indented string 4 different, difference should go back to string 2" x6 y6 d6
        , testS  "indented string 3 different, difference should go forward to string 4 and back to string 2" x7 y7 d7
        , testB  "primitive blocks: item >> item! in 3rd block" a1 b1 dd1
        ]


x1 = ["aaa", "bbb", "ccc"]
y1 = ["aaa", "bxb", "ccc"]
d1 = { commonInitialSegment = ["aaa"], commonTerminalSegment = ["ccc"], middleSegmentInSource = ["bbb"], middleSegmentInTarget = ["bxb"] }


x2 = ["aaa", "bbb", "ccc"]
y2 = ["axa", "bbb", "ccc"]
d2 = { commonInitialSegment = [], commonTerminalSegment = ["bbb","ccc"], middleSegmentInSource = ["aaa"], middleSegmentInTarget = ["axa"] }

x3 = ["aaa", "bbb", "ccc"]
y3 = ["aaa", "bbb", "cxc"]
d3 = { commonInitialSegment = ["aaa","bbb"], commonTerminalSegment = [], middleSegmentInSource = ["ccc"], middleSegmentInTarget = ["cxc"] }


x4 = ["aaa", "  bbb", "  ccc", "ddd"]
y4 = ["aaa", "  bbb", "  cxc", "ddd"]
d4 = { commonInitialSegment = [], commonTerminalSegment = ["ddd"], middleSegmentInSource = ["aaa","  bbb","  ccc"], middleSegmentInTarget = ["aaa","  bbb","  cxc"] }

x5 = ["aaa", "  bbb", "  ccc", "ddd"]
y5 = ["aaa", "  bxb", "  ccc", "ddd"]
d5 = { commonInitialSegment = [], commonTerminalSegment = ["ddd"], middleSegmentInSource = ["aaa","  bbb","  ccc"], middleSegmentInTarget = ["aaa","  bxb","  ccc"] }

x6 = ["000", "aaa", "  bbb", "  ccc", "ddd"]
y6 = ["000", "aaa", "  bbb", "  cxc", "ddd"]
d6 = { commonInitialSegment = ["000"], commonTerminalSegment = ["ddd"], middleSegmentInSource = ["aaa","  bbb","  ccc"], middleSegmentInTarget = ["aaa","  bbb","  cxc"] }


x7 = ["000", "aaa", "  bbb", "  ccc", "ddd"]
y7 = ["000", "aaa", "  bxb", "  ccc", "ddd"]
d7 = { commonInitialSegment = ["000"], commonTerminalSegment = ["ddd"], middleSegmentInSource = ["aaa","  bbb","  ccc"], middleSegmentInTarget = ["aaa","  bxb","  ccc"] }


a1 = toPrimitiveBlocks """
| title
L0 Test

| item
Bread

| item
Cheese

| item
Wine
"""

b1 =  toPrimitiveBlocks """
| title
L0 Test

| item
Bread

| item!
Cheese

| numbered
Wine
"""

dd1 = { commonInitialSegment = [{ args = [], blockType = PBOrdinary, content = ["L0 Test"], indent = 0, lineNumber = 2, name = Just "title", position = 1, properties = Dict.fromList [], sourceText = "L0 Test" },{ args = [], blockType = PBOrdinary, content = ["Bread"], indent = 0, lineNumber = 5, name = Just "item", position = 15, properties = Dict.fromList [], sourceText = "Bread" }], commonTerminalSegment = [{ args = [], blockType = PBParagraph, content = [], indent = 0, lineNumber = 13, name = Nothing, position = 48, properties = Dict.fromList [], sourceText = "" }], middleSegmentInSource = [{ args = [], blockType = PBOrdinary, content = ["Cheese"], indent = 0, lineNumber = 8, name = Just "item", position = 26, properties = Dict.fromList [], sourceText = "Cheese" },{ args = [], blockType = PBOrdinary, content = ["Wine"], indent = 0, lineNumber = 11, name = Just "item", position = 38, properties = Dict.fromList [], sourceText = "Wine" }], middleSegmentInTarget = [{ args = [], blockType = PBOrdinary, content = ["Cheese"], indent = 0, lineNumber = 8, name = Just "item!", position = 26, properties = Dict.fromList [], sourceText = "Cheese" },{ args = [], blockType = PBOrdinary, content = ["Wine"], indent = 0, lineNumber = 11, name = Just "numbered", position = 39, properties = Dict.fromList [], sourceText = "Wine" }] }

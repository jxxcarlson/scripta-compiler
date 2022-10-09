module DiffTests exposing (..)

import Expect exposing (equal)
import Test exposing (Test, describe, test)
import Compiler.Differ as D exposing(DiffRecord)
import Compiler.DifferEq as DE
import Parser.Utility


--- STRINGS

diffS : List String -> List String -> DiffRecord String
diffS = DE.diff (\a b -> a == b) (\a -> String.length (Parser.Utility.getLeadingBlanks a))

testS : String -> List String -> List String -> DiffRecord String -> Test
testS label a b c =
     test label <| \_ -> equal (diffS a b) c

suite : Test
suite =
    Test.only <| describe "Compiler.DifferEq.diff"
        [ testS  "middle string is different" x1 y1 d1
        , testS  "initial string is different" x2 y2 d2
        , testS  "final string is different" x3 y3 d3
        , testS  "indented string 3 different, difference should go back to string 1 (root)" x4 y4 d4
        , testS  "indented string 2 different, difference should go forward to string 3 and back to string 1 (root)" x5 y5 d5
        , testS  "indented string 4 different, difference should go back to string 2" x6 y6 d6
        , testS  "indented string 3 different, difference should go forward to string 4 and back to string 2" x7 y7 d7
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



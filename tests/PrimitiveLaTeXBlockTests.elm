module PrimitiveLaTeXBlockTests exposing (..)

import Expect exposing (equal)
import Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parseLoop, print)
import Test exposing (Test, describe, test)


blockTest : String -> String -> String -> Test
blockTest label sourceText printedResult =
    test label <| \_ -> equal (parseLoop (String.lines sourceText) |> .blocks |> List.map print |> String.join "\n\n") printedResult


suite : Test
suite =
    describe "PrimitiveLaTeXBlock.parse"
        [ blockTest "a1" a1 a1OUT
        ]


a1 =
    """\\begin{A}
abc OK
def
\\end{A}

\\begin{B}
xyz
\\end{B}"""


a1OUT =
    """BLOCK:
Type: Ordinary
Name: B
Level: 0
Status: Finished
Error: none
Line number: 5
Content:
  6: xyz

BLOCK:
Type: Ordinary
Name: A
Level: 0
Status: Finished
Error: none
Line number: 0
Content:
  1: abc OK
  2: def"""

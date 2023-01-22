module PrimitiveBlockTest exposing (..)

import Expect exposing (equal)
import Parser.PrimitiveLaTeXBlock
import Test exposing (Test, describe, test)


lastBlockLevel : String -> Maybe Int
lastBlockLevel source =
    source
        |> String.lines
        |> Parser.PrimitiveLaTeXBlock.parse
        |> List.head
        |> Maybe.map .level


testBlock : String -> String -> Test
testBlock label source =
    test label <| \_ -> equal (lastBlockLevel source) (Just 0)


suite : Test
suite =
    describe "PrimitiveLaTeXBlock parser ... levels"
        [ testBlock "x1" x1
        , testBlock "x2" x2
        , testBlock "x3" x3
        , testBlock "x4" x4
        , testBlock "x5" x5
        ]


x1 =
    """
abc
def
"""


x2 =
    """
abc
def

ghi
jkl
"""


x3 =
    """
\\begin{AAA}
abc

$$
x^2
$$

def
\\end{AAA}

xyz

"""


x4 =
    """
\\begin{AAA}
abc

\\begin{BBB}
x^2
\\end{BBB}

def
\\end{AAA}

xyz

"""


x5 =
    """
\\begin{AAA}

abc

\\begin{BBB}
x^2
\\end{BBB}

def

\\end{AAA}

xyz

"""

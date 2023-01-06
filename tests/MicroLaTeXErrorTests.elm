module MicroLaTeXErrorTests exposing (blockSuite, macroSuite)

import Expect exposing (equal)
import MicroLaTeX.Parser.Pretty
import Parser.PrimitiveLaTeXBlock
import Test exposing (Test, describe, test)


testError : String -> String -> String -> Test
testError label input expectedOutput =
    test label <| \_ -> equal (MicroLaTeX.Parser.Pretty.print input) expectedOutput


testBlockError : String -> String -> String -> Test
testBlockError label input expectedOutput =
    let
        parsed =
            input |> String.lines |> Parser.PrimitiveLaTeXBlock.parse_

        output =
            List.map Parser.PrimitiveLaTeXBlock.printErr parsed.blocks
                |> String.join "\n\n"
    in
    test label <| \_ -> equal output expectedOutput


macroSuite : Test
macroSuite =
    describe "MicroLaTeX Error Tests"
        [ testError "missing right brace" "\\italic{foo" "\\errorHighlight{\\italic{}foo\n\n"
        , testError "naked backslash" "\\" "\\errorHighlight{ \\?}\n\n"
        , testError "complete macro (no error)" "\\foo{bar}" "\\foo{bar}\n\n"
        , testError "extra right brace" "\\foo{bar}}" "\\foo{bar}\\errorHighlight{ extra }?}\n\n"
        , testError "empty argument" "\\foo{}" "\\foo{\\errorHighlight{{??}}}\n\n"
        , testError "extra left brace" "\\foo{{bar}" "\\errorHighlight{\\foo{}\\blue{{bar}\\errorHighlight{}\\errorHighlight{ extra }?}\n\n"
        ]


blockSuite : Test
blockSuite =
    Test.only <|
        describe "MicroLaTeX Block Error Tests"
            [ testBlockError "missmatched tags" missmatchedTags "Missmatched tags: begin(foo) ≠ end(bar)"
            , testBlockError "missing end tag" missingEndTag "missing or mismatched end tag (foo)"
            ]


missingEndTag =
    """
\\begin{foo}
abc

"""


missmatchedTags : String
missmatchedTags =
    """
\\begin{foo}
abc
\\end{bar}
"""

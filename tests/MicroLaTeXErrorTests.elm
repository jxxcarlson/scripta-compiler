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
            input |> String.lines |> Parser.PrimitiveLaTeXBlock.parseLoop

        output =
            List.map Parser.PrimitiveLaTeXBlock.printErr parsed.blocks
                |> String.join "\n\n"
    in
    test label <| \_ -> equal output expectedOutput


macroSuite : Test
macroSuite =
    describe "MicroLaTeX Error Tests"
        [ testError "missing right brace" "\\italic{foo" "\\errorHighlight{\\italic{}foo"
        , testError "naked backslash" "\\" "\\errorHighlight{ \\?}"
        , testError "complete macro (no error)" "\\foo{bar}" "\\foo{bar}"
        , testError "extra right brace" "\\foo{bar}}" "\\foo{bar}\\errorHighlight{ extra }?}"
        , testError "empty argument" "\\foo{}" "\\foo{\\errorHighlight{{??}}}"
        , testError "extra left brace" "\\foo{{bar}" "\\errorHighlight{\\foo{}\\blue{{bar}\\errorHighlight{}\\errorHighlight{ extra }?}"
        ]


blockSuite : Test
blockSuite =
    describe "MicroLaTeX Block Error Tests"
        [ testBlockError "missmatched tags" missmatchedTags "Mismatch: \\begin{foo} ≠ \\end{bar}"
        , testBlockError "missing end tag" missingEndTag "Missing \\end{foo}"
        , testBlockError "missing end tag, code block" codeMissingEndTag "Missing \\end{code}\n\n"
        ]


codeMissingEndTag =
    """
\\begin{code}
abc

  def
  
"""


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

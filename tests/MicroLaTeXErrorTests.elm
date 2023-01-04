module MicroLaTeXErrorTests exposing (macroSuite)

import Expect exposing (equal)
import MicroLaTeX.Parser.Pretty
import Test exposing (Test, describe, test)


testError label input expectedOutput =
    test label <| \_ -> equal (MicroLaTeX.Parser.Pretty.print input) expectedOutput


macroSuite : Test
macroSuite =
    Test.only <|
        describe "MicroLaTeX Error Tests"
            [ testError "middle string is different" "\\italic{foo" "\\errorHighlight{\\italic{}foo\n\n"
            ]

module TransformLaTeXTest exposing (suite)

import Expect exposing (equal)
import MicroLaTeX.Parser.TransformLaTeX exposing (toL0)
import Test exposing (Test, describe, test)


test_ label expr expected =
    test label <| \_ -> equal expr expected


suite : Test
suite =
    describe "TransformLaTeX"
        [ test_ "[]" (toL0 []) []
        , test_ "plain text" (toL0 [ "a b c" ]) [ "a b c" ]
        , test_ "$$" (toL0 [ "$$\nx^2\n$$" ]) [ "$$\nx^2\n$$" ]
        , test_ "env" (toL0 [ "\\begin{theorem}\nLots of primes\n\\end{theorem}" ]) [ "| theorem\nLots of primes\n\\end{theorem}" ]
        , test_ "section" (toL0 [ "\\section{Introduction}" ]) [ "\\section{Introduction}" ]
        , test_ "code1" (toL0 [ "```\n\na := 1\n\n```" ]) [ "```\n\na := 1\n\n```" ]
        , test_ "code" (toL0 [ "\\begin{code}\na := 1\nb := 2\n\\end{code}" ]) [ "|| code" ]
        , test_ "equation" (toL0 [ "\\begin{equation}\nx^1\n\\end{equation}" ]) [ "|| equation" ]
        , test_ "aligned" (toL0 [ "\\begin{aligned}x & 1\\\\y & 2\\end{aligned}" ]) [ "|| aligned" ]
        , test_ "tags" (toL0 [ "\\tags{AAA}" ]) [ "| tags AAA " ]
        , test_ "item" (toL0 [ "\\item\nAAA" ]) [ "| item\nAAA" ]
        , test_ "numbered" (toL0 [ "\\numbered\nAAA" ]) [ "| numbered\nAAA" ]
        , test_ "contents" (toL0 [ "\\contents{Intro}" ]) [ "| contents Intro " ]
        , test_ "contents2" (toL0 [ "\\contents" ]) [ "| contents" ]
        , test_ "bibitem" (toL0 [ "\\bibitem{AA}\nAA, a story about the alphabet" ]) [ "| bibitem AA \nAA, a story about the alphabet" ]
        , test_ "desc" (toL0 [ "\\desc{AA}\nBBB" ]) [ "| desc AA \nBBB" ]
        ]

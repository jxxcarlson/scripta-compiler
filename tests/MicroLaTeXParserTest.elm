module MicroLaTeXParserTest exposing (happy, happy2, primitiveBlock)

import Expect exposing (equal)
import MicroLaTeX.Parser.Pretty
import MicroLaTeX.Parser.Token exposing (Token(..))
import MicroLaTeX.Test
import Test exposing (Test, describe, test)


roundTrip1 text =
    test (text ++ ":1") <| \_ -> equal (MicroLaTeX.Test.check1 text) (Ok text)


roundTrip2 text =
    test (text ++ ":2") <| \_ -> equal (MicroLaTeX.Test.check2 text) (Ok text)


tokenizer text tokens =
    test (text ++ ":token") <| \_ -> equal (MicroLaTeX.Parser.Token.run text) tokens


happy : Test
happy =
    describe "MicroLaTeX Expression, round trip-tests, happy path"
        [ roundTrip1 "abc"
        , roundTrip1 "\\foo{bar}"
        , roundTrip1 "\\foo bar"
        , roundTrip1 "$x^2$"
        , roundTrip1 "\\italic{foo $x^2$ bar}"
        , roundTrip1 "\\link{Waves https://nytimes.com}"
        , roundTrip1 "\\link{foo bar}"
        , roundTrip1 "\\blue{\\italic{abc def ghi}}"
        , roundTrip1 "\\blue{\\italic{abc \\strong{def}}}"
        , roundTrip1 "\\foo{#1}"
        , roundTrip1 "\\foo{#1} = \\bar{#2}"
        , roundTrip1 "\\f{\\foo{#1}}"
        , roundTrip1 "\\f{a = \\foo{b}}"
        , roundTrip1 "\\f{\\foo{a} = b}"
        , roundTrip1 "\\f{\\foo{a} =\\bar{b}}"
        , roundTrip1 "\\blue{#1}, \\red{#2}"
        , roundTrip1 "\\blue{\\italic{aaa} = \\italic{bbb}}"
        , roundTrip1 "\\body{\\foo{#1} = b}"
        , roundTrip1 "\\body{\\foo{#1} =\\bar{#2}}"
        , roundTrip1 "\\blue{\\italic{abc \\strong{def} ghi}}"
        , tokenizer "\\link{foo bar}" (List.reverse <| [ BS { begin = 0, end = 0, id = "0.0", index = 0 }, S "link" { begin = 1, end = 4, id = "1.1", index = 1 }, LB { begin = 5, end = 5, id = "5.1", index = 2 }, S "foo bar" { begin = 6, end = 12, id = "6.12", index = 3 }, RB { begin = 13, end = 13, id = "13.3", index = 4 } ])
        , tokenizer "\\italic{foo $x^2$ bar}" [ RB { begin = 21, end = 21, id = "21.7", index = 8 }, S " bar" { begin = 17, end = 20, id = "17.20", index = 7 }, MathToken { begin = 16, end = 16, id = "16.5", index = 6 }, S "x^2" { begin = 13, end = 15, id = "13.5", index = 5 }, MathToken { begin = 12, end = 12, id = "12.3", index = 4 }, S "foo " { begin = 8, end = 11, id = "8.11", index = 3 }, LB { begin = 7, end = 7, id = "7.1", index = 2 }, S "italic" { begin = 1, end = 6, id = "1.1", index = 1 }, BS { begin = 0, end = 0, id = "0.0", index = 0 } ]
        ]


happy2 : Test
happy2 =
    describe "MicroLaTeX Expression, round trip-tests, unhappy path"
        [ roundTrip1 "\\f{u v w \\foo{#1}}"
        , roundTrip2 "\\foo{bar}{\\aargh{baz}}"
        , roundTrip2 "\\foo{bar}{baz}"
        ]


primitiveBlock : Test
primitiveBlock =
    describe "parse text to primitive blocks"
        [ primitiveBlockRoundTripTest "environments" text0
        , primitiveBlockRoundTripTest "nested environments" text1
        , primitiveBlockRoundTripTest "code1" code1
        , primitiveBlockRoundTripTest "code2" code2
        ]


primitiveBlockRoundTripTest : String -> String -> Test
primitiveBlockRoundTripTest label input =
    test (label ++ ":2") <|
        \_ ->
            equal
                ( MicroLaTeX.Parser.Pretty.roundTripTest input, MicroLaTeX.Parser.Pretty.idempotencyTest input )
                ( True, True )


text0 =
    """
\\begin{A}
abc OK
def
\\end{A}

\\begin{B}
xyz
\\end{B}
"""


text1 =
    """
\\begin{theorem}
Yay!

\\begin{equation}
x^2
\\end{equation}

\\end{theorem}

ABC

XYZ

PQR
"""


code1 =
    """
\\begin{code}
abc
  
  def
  
ghi
\\end{code}
"""


code2 =
    """
\\begin{code}
aaa
$$
x^2
$$
bbb
\\end{code}
"""



--unhappy : Test
--unhappy =
--    describe "L0 Expression, round trip-tests, unhappy path"
--        [ checkErrorHandling "[foo" "[errorHighlight[foo][errorHighlight]?]"
--        ]

module MatchTests exposing (p, suite, suite2, testParser, test_)

import Expect exposing (equal)
import L0.Parser.Expression exposing (parseWithMessages)
import L0.Parser.Match exposing (isReducible)
import L0.Parser.Symbol exposing (Symbol(..))
import Parser.Expr exposing (Expr(..))
import Test exposing (Test, describe, test)


test_ label expr expected =
    test label <| \_ -> equal expr expected


testParser input expected =
    test input <| \_ -> equal (p input) expected


p =
    parseWithMessages 0


suite : Test
suite =
    describe "Match"
        [ test_ "[L, ST, R]" (isReducible [ L, ST, R ]) True
        , test_ "[L, ST, ST, R]" (isReducible [ L, ST, ST, R ]) True
        , test_ "[L,ST,WS,L,ST,ST,R,R]" (isReducible [ L, ST, WS, L, ST, ST, R, R ]) True
        , test_ "[M, ST, M]" (isReducible [ M, ST, M ]) True
        , test_ "[L, L, ST, R]" (isReducible [ L, L, ST, R ]) False
        ]


suite2 : Test
suite2 =
    describe "Parse"
        [ testParser "this is text" ( [ Text "this is text" { begin = 0, end = 11, id = "0.0", index = 0 } ], [] )
        , testParser "aaa [i italic]" ( [ Text "aaa " { begin = 0, end = 3, id = "0.0", index = 0 }, Fun "i" [ Text " italic" { begin = 6, end = 12, id = "0.3", index = 3 } ] { begin = 5, end = 5, id = "0.2", index = 2 } ], [] )
        , testParser "aaa [i bbb [blue ccc] ddd] eee" ( [ Text "aaa " { begin = 0, end = 3, id = "0.0", index = 0 }, Fun "i" [ Text " bbb " { begin = 6, end = 10, id = "0.3", index = 3 }, Fun "blue" [ Text " ccc" { begin = 16, end = 19, id = "0.6", index = 6 } ] { begin = 12, end = 15, id = "0.5", index = 5 }, Text " ddd" { begin = 21, end = 24, id = "0.8", index = 8 } ] { begin = 5, end = 5, id = "0.2", index = 2 }, Text " eee" { begin = 26, end = 29, id = "0.10", index = 10 } ], [] )
        , testParser "math: \\[x^2\\]" ( [ Text "math: " { begin = 0, end = 5, id = "0.0", index = 0 }, Verbatim "math" "x^2" { begin = 6, end = 12, id = "0.1", index = 1 } ], [] )
        ]

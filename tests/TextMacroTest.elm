module TextMacroTest exposing (suite)

import Compiler.TextMacro
    exposing
        ( applyMacroS
        , macroFromL0String
        , listSubst
        , printMacro
        , applyMacroS2
        )
import Expect exposing (equal)
import L0.Test
import Parser.Expr exposing (Expr(..))
import Test exposing (Test, describe, test)


test_ label input output =
    test label <| \_ -> equal input output


exprsToString =
    List.map L0.Test.toString >> String.join ""


suite : Test
suite =
    describe "the Lambda module"
        [ test_ "multiple substitution" (listSubst [ a1, a2 ] [ "x", "y" ] eee |> exprsToString) "[blue[i good]] = [red[b bad]]"
        , test_ "construct, then print macro" (macroFromL0String "[macro f x [blue [i x]]]" |> Maybe.map printMacro) (Just "Macro f, vars: [x], expr:  [blue [i x]]")
        , test_ "apply lambda expr 1" (applyMacroS "[macro f x [blue [bold x]]]" "[f [italic Hello!]]") (Just "[group[blue [bold[italic Hello!]]]]")
        , test_ "apply lambda expr 2" (applyMacroS "[macro f x y [blue [i x]] [red [b y]]]" "[f [var bird] [var frog]]") (Just "[group[blue [i[var bird]]] [red [b[var frog]]]]")
        , test_ "apply lambda expr 3 (microLaTeX)" (applyMacroS2 "\\newcommand{\\foo}{\\blue{#1} = \\red{#2}}" "\\foo{ABC}{DEF}") (Just ("[group[blue ABC] = [red DEF]]"))
        ]


{-|

    From "[i good]"

-}
a1 =
    Fun "i" [ Text " good" { begin = 2, end = 6, id = "0.2", index = 2 } ] { begin = 1, end = 1, id = "0.1", index = 1 }


{-|

    From "[b bad]"

-}
a2 =
    Fun "b" [ Text " bad" { begin = 2, end = 5, id = "0.2", index = 2 } ] { begin = 1, end = 1, id = "0.1", index = 1 }


{-|

    From [ "[blue x]", " = ", "[red y]" ]

-}
eee : List Expr
eee =
    [ Fun "blue" [ Text " x" { begin = 9, end = 10, id = "0.5", index = 5 } ] { begin = 5, end = 8, id = "0.4", index = 4 }, Text " = " { begin = 12, end = 14, id = "0.7", index = 7 }, Fun "red" [ Text " y" { begin = 19, end = 20, id = "0.10", index = 10 } ] { begin = 16, end = 18, id = "0.9", index = 9 } ]

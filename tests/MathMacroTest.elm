module MathMacroTest exposing (suite)

import Expect exposing (equal)
import Parser.MathMacro
    exposing
        ( evalStr
        , makeMacroDict
        , parse
        , parseNewCommand
        , printList
        , printNewCommand
        )
import Result
import Test exposing (Test, describe, test)


testMathExpression str =
    parse str |> Result.map printList


test0 label expr expected=
    test label <| \_ -> equal expr  expected


test_ input =
    test input <| \_ -> equal (testMathExpression input) (Ok input)


testNewCommand input =
    test input <| \_ -> equal (parseNewCommand input |> Result.map printNewCommand) (Ok input)



suite : Test
suite =
    describe "MathExpression module"
        [ test_ "foo"
        , test_ "\\foo"
        , test_ "{\\foo}"
        , test_ "#1"
        , test_ "A_1"
        , test_ "x^2"
        , test_ "x_{#1}"
        , test_ "x^\\alpha"
        , test_ "x^{\\alpha_1}"
        , test_ "\\overline{M_{#1}}"
        , test_ "(x^2) = (y^2)"
        , test0 "nested insdie existing macro" (evalStr d0 t0) t0b
        , testNewCommand "\\newcommand{\\foo}[1]{\\overline{M_{#1}}}"
        , testNewCommand "\\newcommand{\\grst}{\\Gamma^{rs}_t}"
        , testNewCommand "\\newcommand{\\christoffel}[3]{\\ensuremath{\\Gamma^{#1#2}_{#3}}}"
        , testNewCommand "\\newcommand{\\avector}[2]{(#1_1,#1_2,\\ldots,#1_{#2})}"
        , testNewCommand "\\newcommand{\\Ext}{\\operatorname{Ext}}"
        , testNewCommand "\\newcommand{\\case}[1]{\\subsubsection*{#1}}"
        , testNewCommand "\\newcommand{\\ah}{\\hat{a}}"
        , testNewCommand "\\newcommand{\\cA}{\\mathcal{A}}"
        , test "macro expansion 1" <| \_ -> equal (evalStr d "\\foo{x}{y}") "\\alpha({x},{y})"
        , test "macro expansion 2" <| \_ -> equal (evalStr d "x_{\\foo{x}{y}}") "x_{\\alpha({x},{y})}"
        ]


s1 =
    "\\newcommand{\\foo}[2]{\\alpha(#1,#2)}"

s0 =
    "\\newcommand{\\baar}[1]{\\beta_{#1}}"

t0 = "\\frac{\\baar{X}}{\\baar{Y}}"

t0b = "\\frac{\\beta_{{X}}}{\\beta_{{Y}}}"

d0 = makeMacroDict s0


d =
    makeMacroDict s1



-- Dict.fromList [("foo",MacroBody 2 [Macro "alpha" [],MathSymbols "(",Param 1,MathSymbols ",",Param 2,MathSymbols ")"])]
--> parse "\\foo{x}{1} = \\foo{1}{y}"
--Ok [Macro "foo" [Arg [AlphaNum "x"],Arg [MathSymbols "1"]],MathSymbols (" = "),Macro "foo" [Arg [MathSymbols "1"],Arg [AlphaNum "y"]]]
--[Macro "foo" [Arg [AlphaNum "x"],Arg [MathSymbols "1"]],MathSymbols (" = "),Macro "foo" [Arg [MathSymbols "1"],Arg [AlphaNum "y"]]]
-- "\\alpha({x},{1}) = \\alpha({1},{y})"

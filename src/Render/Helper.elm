module Render.Helper exposing (renderWithDefault)

import Compiler.Acc
import Element exposing (Element)
import Element.Font as Font
import Parser.Expr exposing (Expr)
import Render.Expression
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)


renderWithDefault : String -> Int -> Compiler.Acc.Accumulator -> Settings -> List Expr -> List (Element MarkupMsg)
renderWithDefault default count acc settings exprs =
    if List.isEmpty exprs then
        [ Element.el [ Font.color settings.redColor, Font.size 14 ] (Element.text default) ]

    else
        List.map (Render.Expression.render count acc settings) exprs

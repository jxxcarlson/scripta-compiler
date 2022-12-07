module Compiler exposing (..)

import Expression exposing (exprListParser, parse)
import Html exposing (Html)


compile : String -> Html msg
compile str =
    case Expression.parse str of
        Nothing ->
            Html.text "Oops, parse error"

        Just exprList ->
            Html.div [] (List.map Expression.render exprList)

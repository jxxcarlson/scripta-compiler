module Render.Utility exposing
    ( elementAttribute
    , getArg
    , hspace
    , internalLink
    , keyValueDict
    , makeId
    , vspace
    )

import Compiler.ASTTools
import Dict exposing (Dict)
import Element exposing (paddingEach)
import Html.Attributes
import List.Extra
import Maybe.Extra
import Parser.Expr


getArg : String -> Int -> List String -> String
getArg default index args =
    case List.Extra.getAt index args of
        Nothing ->
            default

        Just a ->
            a


vspace : Int -> Int -> Element.Attribute msg
vspace top bottom =
    paddingEach { left = 0, right = 0, top = top, bottom = bottom }


hspace : Int -> Int -> Element.Attribute msg
hspace left right =
    paddingEach { left = left, right = right, top = 0, bottom = 0 }


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


makeId : List Parser.Expr.Expr -> Element.Attribute msg
makeId exprs =
    elementAttribute "id"
        (Compiler.ASTTools.stringValueOfList exprs |> String.trim |> makeSlug)


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " ""




keyValueDict : List String -> Dict String String
keyValueDict strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.map pairFromList
        |> Maybe.Extra.values
        |> Dict.fromList


pairFromList : List String -> Maybe ( String, String )
pairFromList strings =
    case strings of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


elementAttribute : String -> String -> Element.Attribute msg
elementAttribute key value =
    Element.htmlAttribute (Html.Attributes.attribute key value)

module Render.Utility exposing
    ( elementAttribute
    , getArg
    , getVerbatimContent
    , highlightElement
    , hspace
    , idAttribute
    , internalLink
    , keyValueDict
    , makeId
    , sendLineNumberOnClick
    , textWidth
    , vspace
    )

import Compiler.ASTTools
import Dict exposing (Dict)
import Either
import Element exposing (paddingEach)
import Element.Background as Background
import Element.Events as Events
import Html.Attributes
import List.Extra
import Maybe.Extra
import Parser.Block
import Parser.Expr
import Render.Msg exposing (MarkupMsg(..))
import Utility


textWidth : String -> Float
textWidth str_ =
    let
        -- \\[a-z]*([^a-z])
        str =
            str_ |> String.words |> List.map compress |> String.join " "

        letters =
            String.split "" str
    in
    letters |> List.map charWidth |> List.sum


charWidth : String -> Float
charWidth c =
    Dict.get c charDict |> Maybe.withDefault 1.0


compress string =
    string
        ++ " "
        |> Utility.userReplace "\\\\[a-z].*[^a-zA-Z0-9]" (\_ -> "a")
        |> Utility.userReplace "\\[A-Z].*[^a-zA-Z0-9]" (\_ -> "A")
        |> String.trim


charDict : Dict String Float
charDict =
    Dict.fromList
        [ ( "a", 1.0 )
        , ( "b", 1.0 )
        , ( "c", 1.0 )
        , ( "d", 1.0 )
        , ( "e", 1.0 )
        , ( "f", 1.0 )
        , ( "g", 1.0 )
        , ( "h", 1.0 )
        , ( "i", 1.0 )
        , ( "j", 1.0 )
        , ( "k", 1.0 )
        , ( "l", 1.0 )
        , ( "m", 1.0 )
        , ( "n", 1.0 )
        , ( "o", 1.0 )
        , ( "p", 1.0 )
        , ( "q", 1.0 )
        , ( "r", 1.0 )
        , ( "s", 1.0 )
        , ( "t", 1.0 )
        , ( "u", 1.0 )
        , ( "v", 1.0 )
        , ( "w", 1.0 )
        , ( "x", 1.0 )
        , ( "y", 1.0 )
        , ( "z", 1.0 )
        , ( "A", 2.0 )
        , ( "B", 2.0 )
        , ( "C", 2.0 )
        , ( "D", 2.0 )
        , ( "E", 2.0 )
        , ( "F", 2.0 )
        , ( "G", 2.0 )
        , ( "H", 2.0 )
        , ( "I", 2.0 )
        , ( "J", 2.0 )
        , ( "K", 2.0 )
        , ( "L", 2.0 )
        , ( "M", 2.0 )
        , ( "N", 2.0 )
        , ( "O", 2.0 )
        , ( "P", 2.0 )
        , ( "Q", 2.0 )
        , ( "R", 2.0 )
        , ( "S", 2.0 )
        , ( "T", 2.0 )
        , ( "U", 2.0 )
        , ( "V", 2.0 )
        , ( "W", 2.0 )
        , ( "X", 2.0 )
        , ( "Y", 2.0 )
        , ( "Z", 2.0 )
        , ( "$", 1.0 )
        ]


getVerbatimContent : Parser.Block.ExpressionBlock -> String
getVerbatimContent (Parser.Block.ExpressionBlock { content }) =
    case content of
        Either.Left str ->
            str

        Either.Right _ ->
            ""


sendLineNumberOnClick : Int -> Int -> Element.Attribute MarkupMsg
sendLineNumberOnClick firstLineNumber numberOfLines =
    Events.onClick (SendLineNumber { begin = firstLineNumber, end = firstLineNumber + numberOfLines })


idAttribute : Int -> Element.Attribute msg
idAttribute k =
    elementAttribute "id" (String.fromInt k)


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


highlightElement : Int -> Int -> String -> String -> List (Element.Attribute MarkupMsg)
highlightElement firstLineNumber lastLineNumber id selectedId =
    if id == selectedId then
        [ Events.onClick (SendLineNumber { begin = firstLineNumber, end = lastLineNumber }), Background.color (Element.rgb 0.8 0.8 1.0) ]

    else
        [ Events.onClick (SendLineNumber { begin = firstLineNumber, end = lastLineNumber }) ]


leftPadding =
    Element.paddingEach { left = 45, right = 0, top = 0, bottom = 0 }

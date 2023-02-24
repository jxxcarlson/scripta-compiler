module Render.Sync exposing
    ( highlightIfIdIsSelected
    , highlightIfIdSelected
    , highlighter
    , rightLeftSyncHelper
    )

import Element exposing (Element, paddingEach)
import Element.Background as Background
import Element.Events as Events
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings


highlightIfIdIsSelected firstLineNumber numberOfLines settings =
    if String.fromInt firstLineNumber == settings.selectedId then
        [ rightLeftSyncHelper firstLineNumber (firstLineNumber + numberOfLines)
        , Background.color (Element.rgb 0.8 0.8 1.0)
        ]

    else
        []


rightLeftSyncHelper : Int -> Int -> Element.Attribute MarkupMsg
rightLeftSyncHelper firstLineNumber numberOfLines =
    Events.onClick (SendLineNumber { begin = firstLineNumber, end = firstLineNumber + numberOfLines })


highlighter : List String -> List (Element.Attr () msg) -> List (Element.Attr () msg)
highlighter args attrs =
    if List.member "highlight" args then
        Background.color selectedColor :: attrs

    else
        attrs


highlightIfIdSelected : String -> Render.Settings.Settings -> List (Element.Attr () msg) -> List (Element.Attr () msg)
highlightIfIdSelected id settings attrs =
    if id == settings.selectedId then
        Background.color selectedColor :: attrs

    else
        attrs


selectedColor : Element.Color
selectedColor =
    Element.rgb 0.9 0.9 1.0

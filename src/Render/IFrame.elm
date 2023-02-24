module Render.IFrame exposing (render)

import Bool.Extra
import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Element exposing (Element)
import Html
import Html.Attributes
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Utility
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Sync
import Render.Utility


render : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
render count acc settings ((ExpressionBlock { lineNumber, numberOfLines, properties }) as block) =
    case parseIFrame (Render.Utility.getVerbatimContent block) of
        Nothing ->
            Element.el [] (Element.text "Error parsing iframe or unregistered src")

        Just iframeProperties ->
            let
                w =
                    String.toInt iframeProperties.width |> Maybe.withDefault 400

                caption_ =
                    Dict.get "caption" properties

                label_ =
                    Dict.get "figure" properties

                figureLabel =
                    case ( label_, caption_ ) of
                        ( Just label, Just caption ) ->
                            "Figure " ++ label ++ ". " ++ caption

                        ( Just label, Nothing ) ->
                            "Figure " ++ label

                        ( Nothing, Just caption ) ->
                            caption

                        ( Nothing, Nothing ) ->
                            ""
            in
            Element.column
                [ Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
                , Render.Utility.idAttribute lineNumber
                , Element.width (Element.px w)
                ]
                [ Html.iframe
                    [ Html.Attributes.src <| iframeProperties.src
                    , Html.Attributes.style "border" "none"
                    , Html.Attributes.style "width" (iframeProperties.width ++ "px")
                    , Html.Attributes.style "height" (iframeProperties.height ++ "px")
                    ]
                    []
                    |> Element.html
                , Element.row [ Element.centerX, Element.paddingXY 0 12 ] [ Element.text figureLabel ]
                ]


parseIFrame : String -> Maybe { width : String, height : String, src : String }
parseIFrame str =
    let
        src_ =
            Parser.Utility.parseItem "src" str

        width_ =
            Parser.Utility.parseItem "width" str

        height_ =
            Parser.Utility.parseItem "height" str
    in
    case ( src_, width_, height_ ) of
        ( Just src, Just width, Just height ) ->
            if validSrc src then
                Just { width = width, height = height, src = src }

            else
                Nothing

        _ ->
            Nothing


allowedIFrameSrcList =
    [ "https://www.desmos.com/calculator/", "https://q.uiver.app/" ]


validSrc : String -> Bool
validSrc src =
    List.map (\src_ -> String.contains src_ src) allowedIFrameSrcList |> Bool.Extra.any

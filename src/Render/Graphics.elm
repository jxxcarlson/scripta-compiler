module Render.Graphics exposing (image, quiver, svg, tikz)

import Compiler.ASTTools as ASTTools
import Compiler.Acc exposing (Accumulator)
import Dict
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, px, rgb255, spacing)
import Element.Font as Font
import Parser.Expr exposing (Expr)
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Render.Utility
import SvgParser


red : Element.Color
red =
    rgb255 255 0 0


type alias ImageParameters msg =
    { caption : Element msg
    , description : String
    , placement : Element.Attribute msg
    , width : Element.Length
    , url : String
    }


image : Render.Settings.Settings -> List Expr -> Element msg
image settings body =
    let
        params =
            body |> argumentsFromAST |> imageParameters settings

        inner =
            column [ spacing 8, Element.width (px settings.width), params.placement, Element.paddingXY 0 18 ]
                [ Element.image [ Element.width params.width, params.placement ]
                    { src = params.url, description = params.description }
                , el [ params.placement ] params.caption
                ]
    in
    Element.newTabLink []
        { url = params.url
        , label = inner
        }


svg : Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg
svg _ _ settings _ _ str =
    case SvgParser.parse str of
        Ok html_ ->
            Element.column
                [ Element.paddingEach { left = 0, right = 0, top = 24, bottom = 0 }
                , Element.width (Element.px settings.width)
                ]
                [ Element.column [ Element.centerX ] [ html_ |> Element.html ]
                ]

        Err _ ->
            Element.el [] (Element.text "SVG parse error")


{-| Create elements from HTML markup. On parsing error, output no elements.
-}
tikz : Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg
tikz _ _ settings _ _ str =
    let
        maybePair =
            case String.split "---" str of
                a :: b :: [] ->
                    Just ( a, b )

                _ ->
                    Nothing
    in
    case maybePair of
        Nothing ->
            Element.el [ Font.size 16, Font.color red ] (Element.text "Something is wrong")

        Just ( imageData, _ ) ->
            let
                params =
                    String.words imageData |> imageParameters settings
            in
            Element.column [ Element.spacing 8, Element.width (Element.px settings.width), params.placement, Element.paddingXY 0 18 ]
                [ Element.image [ Element.width params.width, params.placement ]
                    { src = params.url, description = params.description }
                , Element.el [ params.placement ] params.caption
                ]


quiver : Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg
quiver _ _ settings arguments _ str =
    let
        -- arguments: ["width:250","caption:Fig","1"]
        args : { caption : Element msg, description : String, placement : Element.Attribute a, width : Element.Length }
        args =
            parameters settings arguments

        maybePair =
            case String.split "---" str of
                a :: b :: [] ->
                    Just ( a, b )

                _ ->
                    Nothing
    in
    case maybePair of
        Nothing ->
            Element.el [ Font.size 16, Font.color red ] (Element.text "Something is wrong")

        Just ( imageData, _ ) ->
            let
                params =
                    String.words imageData |> imageParameters settings
            in
            Element.column [ Element.spacing 8, Element.width (Element.px settings.width), params.placement, Element.paddingXY 0 18 ]
                [ Element.image [ Element.width args.width, params.placement ]
                    { src = params.url, description = params.description }
                , Element.el [ params.placement ] args.caption
                ]


argumentsFromAST : List Expr -> List String
argumentsFromAST body =
    ASTTools.exprListToStringList body |> List.map String.words |> List.concat


imageParameters : Render.Settings.Settings -> List String -> ImageParameters msg
imageParameters settings arguments =
    let
        url =
            List.head arguments |> Maybe.withDefault "no-image"

        remainingArguments =
            List.drop 1 arguments

        keyValueStrings_ =
            List.filter (\s -> String.contains ":" s) remainingArguments

        keyValueStrings : List String
        keyValueStrings =
            List.filter (\s -> not (String.contains "caption" s)) keyValueStrings_

        captionLeadString =
            List.filter (\s -> String.contains "caption" s) keyValueStrings_
                |> String.join ""
                |> String.replace "caption:" ""

        captionPhrase =
            (captionLeadString :: List.filter (\s -> not (String.contains ":" s)) remainingArguments) |> String.join " "

        dict =
            Render.Utility.keyValueDict keyValueStrings

        description : String
        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption : Element msg
        caption =
            if captionPhrase == "" then
                Element.none

            else
                Element.row [ placement, Element.width Element.fill ] [ el [ Element.width Element.fill ] (Element.text captionPhrase) ]

        displayWidth =
            settings.width

        width : Element.Length
        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just "fill" ->
                    Element.fill

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    centerX

                Just "left" ->
                    alignLeft

                Just "right" ->
                    alignRight

                Just "center" ->
                    centerX

                _ ->
                    centerX
    in
    { caption = caption, description = description, placement = placement, width = width, url = url }



-- parameters : Render.Settings.Settings -> List String -> ImageParameters msg


parameters settings arguments =
    let
        keyValueStrings_ =
            List.filter (\s -> String.contains ":" s) arguments

        keyValueStrings : List String
        keyValueStrings =
            List.filter (\s -> not (String.contains "caption" s)) keyValueStrings_

        captionLeadString =
            List.filter (\s -> String.contains "caption" s) keyValueStrings_
                |> String.join ""
                |> String.replace "caption:" ""

        captionPhrase =
            (captionLeadString :: List.filter (\s -> not (String.contains ":" s)) arguments) |> String.join " "

        dict =
            Render.Utility.keyValueDict keyValueStrings

        description : String
        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption : Element msg
        caption =
            if captionPhrase == "" then
                Element.none

            else
                Element.row [ placement, Element.width Element.fill ] [ el [ Element.width Element.fill ] (Element.text captionPhrase) ]

        displayWidth =
            settings.width

        width : Element.Length
        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just "fill" ->
                    Element.fill

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    centerX

                Just "left" ->
                    alignLeft

                Just "right" ->
                    alignRight

                Just "center" ->
                    centerX

                _ ->
                    centerX
    in
    { caption = caption, description = description, placement = placement, width = width }

module Render.Math exposing
    ( DisplayMode(..)
    , aligned
    , displayedMath
    , equation
    , mathText
    )

import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import Parser.Block exposing (ExpressionBlock(..))
import Parser.MathMacro
import Parser.TextMacro
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Sync
import Render.Utility


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


leftPadding =
    Element.paddingEach { left = 0, right = 0, top = 0, bottom = 0 }


displayedMath : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
displayedMath count acc settings ((ExpressionBlock { id, args, lineNumber, numberOfLines, error }) as block) =
    let
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines (getContent block)
                |> List.filter (\line -> not (String.left 2 (String.trim line) == "$$"))
                |> List.filter (\line -> not (String.left 6 line == "[label"))
                |> List.filter (\line -> line /= "")
                |> List.map (Parser.MathMacro.evalStr acc.mathMacroDict)
    in
    Element.column (Render.Sync.rightLeftSyncHelper lineNumber numberOfLines :: [])
        [ Element.el (Render.Sync.highlighter args [ Element.centerX ]) (mathText count w id DisplayMathMode (filteredLines |> String.join "\n")) ]


getContent : ExpressionBlock -> String
getContent (ExpressionBlock { content }) =
    case content of
        Left str ->
            str

        Right _ ->
            ""


equation : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
equation count acc settings ((ExpressionBlock { lineNumber, numberOfLines, id, args, error, properties }) as block) =
    let
        --_ =
        --    Debug.log "equation (id, selectedId)" ( id, settings.selectedId )
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines (getContent block)
                |> List.filter (\line -> not (String.left 2 line == "$$") && not (String.left 6 line == "[label") && not (line == "end"))
                |> List.map (Parser.MathMacro.evalStr acc.mathMacroDict)

        content =
            String.join "\n" filteredLines

        -- TODO: changed 45 -> 0
    in
    Element.column []
        [ Element.row
            (Render.Sync.rightLeftSyncHelper lineNumber numberOfLines :: [ Element.width (Element.px settings.width), Render.Utility.elementAttribute "id" id ])
            [ Element.el (Render.Sync.highlightIfIdSelected id settings (Render.Sync.highlighter args [ Element.centerX ])) (mathText count w id DisplayMathMode content)
            , putLabel settings.display content properties settings.longEquationLimit
            ]
        ]


putLabel display content properties longEquationLimit_ =
    let
        longEquationLimit =
            case display of
                Render.Settings.DefaultDisplay ->
                    longEquationLimit_

                Render.Settings.PhoneDisplay ->
                    0.9 * longEquationLimit_
    in
    if Render.Utility.textWidth content > longEquationLimit then
        Element.none

    else
        -- TODO: is this fixed?
        -- Element.el [ Font.size 12 ] (Element.text <| "(" ++ (getLabel "equation" properties |> Debug.log "EQ NO (2)") ++ ")")
        Element.el [ Font.size 12 ] (Element.text <| "(" ++ getLabel "equation" properties ++ ")")


getCounter : String -> Dict String Int -> String
getCounter counterName dict =
    Dict.get counterName dict |> Maybe.withDefault 0 |> String.fromInt


getLabel : String -> Dict String String -> String
getLabel label dict =
    Dict.get label dict |> Maybe.withDefault ""


aligned : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
aligned count acc settings ((ExpressionBlock { lineNumber, numberOfLines, id, args, properties, error }) as block) =
    Element.column []
        [ Element.row [ Element.width (Element.px settings.width), Render.Utility.elementAttribute "id" id ]
            [ Element.el [ Element.centerX ] (aligned_ count acc settings args lineNumber numberOfLines id (getContent block))
            , putLabel settings.display (getContent block) properties settings.longEquationLimit
            ]
        ]


aligned_ : Int -> Accumulator -> Settings -> List String -> Int -> Int -> String -> String -> Element MarkupMsg
aligned_ count acc settings args lineNumber numberOfLines id str =
    let
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines str
                |> List.filter (\line -> not (String.left 6 line == "[label") && not (line == ""))

        deleteTrailingSlashes str_ =
            if String.right 2 str_ == "\\\\" then
                String.dropRight 2 str_

            else
                str_

        adjustedLines_ =
            List.map (deleteTrailingSlashes >> Parser.MathMacro.evalStr acc.mathMacroDict) filteredLines
                |> List.filter (\line -> line /= "")
                |> List.map (\line -> line ++ "\\\\")

        adjustedLines =
            "\\begin{aligned}" :: adjustedLines_ ++ [ "\\end{aligned}" ]

        content =
            String.join "\n" adjustedLines
    in
    Element.column (Render.Sync.rightLeftSyncHelper lineNumber numberOfLines :: [])
        [ Element.el (Render.Sync.highlightIfIdSelected id settings (Render.Sync.highlighter args [ Element.centerX ])) (mathText count w id DisplayMathMode content) ]


mathText : Int -> String -> String -> DisplayMode -> String -> Element msg
mathText generation width id displayMode content =
    -- TODO Track this down at the source.
    Html.Keyed.node "span"
        [ HA.style "padding-top" "14px"
        , HA.style "padding-bottom" "14px"
        , HA.id id
        , HA.style "width" width
        ]
        [ ( String.fromInt generation, mathText_ displayMode (eraseLabeMacro content) )
        ]
        |> Element.html


eraseLabeMacro content =
    content |> String.lines |> List.map (Parser.TextMacro.eraseLeadingMacro "label") |> String.join "\n"


mathText_ : DisplayMode -> String -> Html msg
mathText_ displayMode content =
    Html.node "math-text"
        -- active meta selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker meta
        -- , HA.id (makeId meta)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True

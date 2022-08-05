module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import File.Download
import Browser.Dom
import Button
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Render.Msg exposing (MarkupMsg)
import Scripta.API
import Scripta.Language exposing (Language(..))
import Task
import Text
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , count : Int
    , editRecord : Scripta.API.EditRecord
    , language : Language
    , documentType : DocumentType
    , currentTime : Time.Posix
    }


type DocumentType
    = InfoDocument
    | Example


type Msg
    = NoOp
    | InputText String
    | Render MarkupMsg
    | SetLanguage Language
    | Info
    | Export


type alias Flags =
    {}


settings counter =
    { windowWidth = 500
    , counter = counter
    , selectedId = "--"
    , selectedSlug = Nothing
    , scale = 0.8
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = Text.microLaTeXDemo
      , count = 0
      , editRecord = Scripta.API.init Dict.empty MicroLaTeXLang Text.microLaTeXDemo
      , language = MicroLaTeXLang
      , documentType = Example
      , currentTime = Time.millisToPosix 0
      }
    , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model
                | input = str
                , count = model.count + 1
                , editRecord = Scripta.API.update model.editRecord str
              }
            , Cmd.none
            )

        SetLanguage lang ->
            let
                docText =
                    case lang of
                        L0Lang ->
                            Text.l0Demo

                        MicroLaTeXLang ->
                            Text.microLaTeXDemo

                        XMarkdownLang ->
                            Text.xMarkdown

                        PlainTextLang ->
                            ""
            in
            ( { model
                | language = lang
                , editRecord = Scripta.API.init Dict.empty lang docText
                , input = docText
                , count = model.count + 1
                , documentType = Example
              }
            , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
            )

        Info ->
            ( { model
                | language = L0Lang
                , editRecord = Scripta.API.init Dict.empty L0Lang Text.info
                , input = Text.info
                , count = model.count + 1
                , documentType = InfoDocument
              }
            , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
            )

        Export ->
            let
                defaultSettings = Scripta.API.defaultSettings
                exportSettings = { defaultSettings | isStandaloneDocument = True }

                exportText = Scripta.API.export model.currentTime exportSettings model.editRecord.parsed

                fileName = Scripta.API.fileNameForExport model.editRecord.parsed 
            in
            (model, download fileName exportText)

        Render _ ->
            ( model, Cmd.none )


download : String -> String -> Cmd msg
download fileName fileContents =
  File.Download.string fileName "application/x-tex" fileContents
--
-- VIEW
--


noFocus : FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    layoutWith { options = [ focusStyle noFocus ] }
        [ bgGray 0.2 ]
        (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px 1200), height (px 650) ]
            [ -- title "Compiler Demo"
              row [ spacing 18 ]
                [ inputText model
                , displayRenderedText model
                , controls model
                ]
            ]
        ]


controls model =
    column [ alignTop, spacing 18, paddingXY 16 22 ]
        [ setLanguageButton "L0" model.documentType L0Lang model.language
        , setLanguageButton "MicroLaTeX" model.documentType MicroLaTeXLang model.language
        , setLanguageButton "XMarkdown" model.documentType XMarkdownLang model.language
        , el [ paddingXY 0 40 ] (infoButton model.documentType)
        , exportButton
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


displayRenderedText : Model -> Element Msg
displayRenderedText model =
    column [ spacing 8, Font.size 14 ]
        [ el [ fontGray 0.9 ] (text "Rendered Text")
        , outputDisplay_ model
        ]


outputDisplay_ : Model -> Element Msg
outputDisplay_ model =
    column
        [ spacing 18
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , width (px 500)
        , height (px 600)
        , paddingXY 16 32
        , scrollbarY
        , htmlId "scripta-output"
        ]
        (Scripta.API.render (settings model.count) model.editRecord |> List.map (Element.map Render))


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px 500), height (px 600), Font.size 14, htmlId "input-text" ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
        , spellcheck = False
        }



-- VIEWPORT


htmlId : String -> Attribute msg
htmlId str =
    htmlAttribute (Html.Attributes.id str)


jumpToTop : String -> Cmd Msg
jumpToTop id =
    Browser.Dom.getViewportOf id
        |> Task.andThen (\info -> Browser.Dom.setViewportOf id 0 0)
        |> Task.attempt (\_ -> NoOp)



-- BUTTONS


buttonWidth =
    105


exportButton :  Element Msg
exportButton  =
    Button.template
        { tooltipText = "Export text to standard LaTeX"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = Export
        , label = "Export"
        }


infoButton : DocumentType -> Element Msg
infoButton documentType =
    let
        bgColor =
            case documentType of
                InfoDocument ->
                    darkRed

                Example ->
                    gray
    in
    Button.template
        { tooltipText = "Info on the Scripta compiler"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = Info
        , label = "About"
        }


setLanguageButton : String -> DocumentType -> Language -> Language -> Element Msg
setLanguageButton label documentType language currentLanguage =
    let
        bgColor =
            if language == currentLanguage && documentType == Example then
                darkRed

            else
                gray
    in
    Button.template
        { tooltipText = "Set the markup language"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = SetLanguage language
        , label = label
        }


darkRed : Color
darkRed =
    rgb255 140 0 0


gray : Color
gray =
    rgb255 60 60 60


white =
    rgb255 255 255 255



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.4
    , paddingXY 20 20
    ]

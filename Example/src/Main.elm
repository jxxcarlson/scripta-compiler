module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}


import Browser
import Browser.Dom
import Html.Attributes
import Task
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Button
import Element.Input as Input
import Scripta.API
import Scripta.Language exposing(Language(..))
import Dict
import Render.Msg exposing (MarkupMsg)
import Text

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {  input : String
     , count : Int
     , editRecord : Scripta.API.EditRecord
     , language : Language
    }


type Msg
    = NoOp
    | InputText String
    | Render MarkupMsg
    | SetLanguage Language
    | Info



type alias Flags =
    {}

settings counter =
    { windowWidth = 500
    , counter = counter
    , selectedId  = "--"
    , selectedSlug = Nothing
    , scale = 0.8
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = Text.microLaTeXDemo
      , count = 0
      , editRecord = Scripta.API.init  Dict.empty  MicroLaTeXLang  Text.microLaTeXDemo
      , language = MicroLaTeXLang
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | input = str
               , count = model.count + 1
               , editRecord = Scripta.API.update model.editRecord str
              }
               , Cmd.none )

        SetLanguage lang ->
            let
              docText =  case lang of
                    L0Lang -> Text.l0Demo
                    MicroLaTeXLang -> Text.microLaTeXDemo
                    XMarkdownLang -> Text.xMarkdown
                    PlainTextLang -> ""
            in
            (  {model | language = lang
                       , editRecord = Scripta.API.init Dict.empty  lang  docText
                       , input  = docText
                       , count = model.count + 1
                 }
             , Cmd.batch [jumpToTop "scripta-output",jumpToTop "input-text"]
            )

        Info ->

            (  {model | language = L0Lang
                       , editRecord = Scripta.API.init Dict.empty  L0Lang  Text.info
                       , input  = Text.info
                       , count = model.count + 1
                 }
             , Cmd.batch [jumpToTop "scripta-output",jumpToTop "input-text"]
            )

        Render _ -> (model, Cmd.none)




--
-- VIEW
--

noFocus : FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


fontGray g = Font.color (Element.rgb g g g )
bgGray g =  Background.color (Element.rgb g g g)

view : Model -> Html Msg
view model =
        layoutWith { options = [ focusStyle noFocus ] }
            [ bgGray 0.2 ]
              (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px 1200), height (px 650)]
            [ -- title "Compiler Demo"
            row [spacing 18] [
              inputText model
            , displayRenderedText model
            , controls model
            ]


            ]
        ]
controls model = column [alignTop, spacing 18,  paddingXY 16 22] [
     setLanguageButton "L0" L0Lang model.language
   , setLanguageButton "MicroLaTeX" MicroLaTeXLang model.language
   , setLanguageButton "XMarkdown" XMarkdownLang model.language
   , el [paddingXY 0 40] (infoButton)

   ]

title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]



displayRenderedText : Model -> Element Msg
displayRenderedText model =
    column [ spacing 8 , Font.size 14]
        [ el [fontGray 0.9] (text "Rendered Text")
        , outputDisplay_ model]

outputDisplay_ : Model -> Element Msg
outputDisplay_ model =
    column [ spacing 18
             , Background.color (Element.rgb 1.0 1.0 1.0)
            , width (px 500)
            , height (px 600)
            , paddingXY 16 32
            , scrollbarY
            , htmlId "scripta-output"]
         (Scripta.API.render (settings model.count) model.editRecord |> List.map (Element.map Render))


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px 500), height (px 600)  , Font.size 14 , htmlId "input-text"]
     { onChange = InputText
           , text = model.input
           , placeholder = Nothing
           , label = Input.labelAbove [fontGray 0.9] <| el [] (text "Source text")
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


buttonWidth = 105

infoButton : Element Msg
infoButton  =
        Button.template
            { tooltipText = "Info on the Scripta compiler"
            , tooltipPlacement = above
            , attributes = [ Font.color white, Background.color gray  ,  width (px buttonWidth)]
            , msg = Info
            , label = "Info"

            }

setLanguageButton : String -> Language -> Language -> Element Msg
setLanguageButton label language currentLanguage =
    let
        bgColor = if language == currentLanguage then
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
gray = rgb255 60 60 60

white = rgb255 255 255 255
--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.4
    , paddingXY 20 20
    ]



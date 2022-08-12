module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is revers
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Render.Msg exposing (MarkupMsg)
import Scripta.API
import Scripta.Language exposing (Language(..))


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { sourceText : String
    , count : Int
    , language : Language
    }


type Msg
    = NoOp
    | InputText String
    | Render MarkupMsg


type alias Flags =
    {}


displaySettings : Int -> Scripta.API.DisplaySettings
displaySettings counter =
    { windowWidth = 500
    , counter = counter
    , selectedId = "--"
    , selectedSlug = Nothing
    , scale = 0.8
    }


initialText =
    """
Pythagoras says: $a^2 + b^2 = c^2$

This \\strong{will} be on the test:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

"""


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = initialText
      , count = 0
      , language = MicroLaTeXLang
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model
                | sourceText = str
                , count = model.count + 1
              }
            , Cmd.none
            )

        Render _ ->
            ( model, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    layoutWith { options = [] }
        [ bgGray 0.2 ]
        (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 18, width (px 1200), height (px 650) ]
            [ -- title "Compiler Demo"
              row [ spacing 18 ]
                [ inputText model
                , displayRenderedText model
                ]
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


displayRenderedText : Model -> Element Msg
displayRenderedText model =
    column [ spacing 8, Font.size 14 ]
        [ el [ fontGray 0.9 ] (text "Rendered Text")
        , column
            [ spacing 18
            , Background.color (Element.rgb 1.0 1.0 1.0)
            , width (px 500)
            , height (px 600)
            , paddingXY 16 32
            , scrollbarY
            ]
            (Scripta.API.compile (displaySettings model.count) MicroLaTeXLang model.sourceText |> List.map (Element.map Render))
        ]


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px 500), height (px 600), Font.size 14 ]
        { onChange = InputText
        , text = model.sourceText
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
        , spellcheck = False
        }


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.4
    , paddingXY 20 20
    ]

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

| theorem (Whatever)
Ther are infintely many primes

This \\strong{will} be on the test:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

|| image width:200
data:image/jpeg;base64,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

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

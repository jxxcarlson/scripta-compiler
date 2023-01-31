module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import SimpleMarkup


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
    }


type Msg
    = NoOp
    | InputText String


type alias Flags =
    {}


initialText =
    """
Pythagoras says that $a^2 + b^2 = c^2$. In calculus class, we all learned that

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

"""


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = initialText
      , count = 0
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



--
-- VIEW
--


view : Model -> Html Msg
view model =
    layoutWith { options = [] }
        []
        (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 18, width (px 1200), height (px 650) ]
            [ title "KaTeX Demo"
            , SimpleMarkup.compile model.sourceText |> Element.html
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


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
    , paddingXY 20 20
    ]

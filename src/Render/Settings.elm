module Render.Settings exposing
    ( Display(..)
    , Settings
    , blueColor
    , codeColor
    , defaultSettings
    , leftIndent
    , leftIndentation
    , leftRightIndentation
    , makeSettings
    , maxHeadingFontSize
    , redColor
    , topMarginForChildren
    , wideLeftIndentation
    , windowWidthScale
    )

import Element


type alias Settings =
    { paragraphSpacing : Int
    , selectedId : String
    , display : Display
    , longEquationLimit : Float
    , selectedSlug : Maybe String
    , showErrorMessages : Bool
    , showTOC : Bool
    , titleSize : Int
    , width : Int
    , backgroundColor : Element.Color
    , titlePrefix : String
    , isStandaloneDocument : Bool
    }


type Display
    = DefaultDisplay
    | PhoneDisplay


defaultSettings : Settings
defaultSettings =
    makeSettings "" Nothing 1 600 100


makeSettings : String -> Maybe String -> Float -> Int -> Float -> Settings
makeSettings id selectedSlug scale windowWidth longEquationLimit =
    { width = round (scale * toFloat windowWidth)
    , titleSize = 30
    , paragraphSpacing = 28
    , display = DefaultDisplay
    , longEquationLimit = 0.1 * (windowWidth |> toFloat)
    , showTOC = True
    , showErrorMessages = False
    , selectedId = id
    , selectedSlug = selectedSlug
    , backgroundColor = Element.rgb 1 1 1
    , titlePrefix = ""
    , isStandaloneDocument = False
    }


codeColor =
    Element.rgb255 0 0 210


windowWidthScale =
    0.3


maxHeadingFontSize : Float
maxHeadingFontSize =
    32


leftIndent =
    18


topMarginForChildren =
    6


leftIndentation =
    Element.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


wideLeftIndentation =
    Element.paddingEach { left = 54, right = 0, top = 0, bottom = 0 }


leftRightIndentation =
    Element.paddingEach { left = 18, right = 8, top = 0, bottom = 0 }


redColor =
    Element.rgb 0.7 0 0


blueColor =
    Element.rgb 0 0 0.9

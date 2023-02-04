module Render.Settings exposing
    ( Display(..)
    ,  Settings
       --, blueColor

    , defaultSettings
    , makeSettings
    , maxHeadingFontSize
    , redColor
    ,  topMarginForChildren
       --, wideLeftIndentation
       --, windowWidthScale

    )

{-| The Settings record holds information needed to render a
parsed document. For example, the renderer needs to
know the width of the window in which the document
is to be displayed. This is given by the `.width` field.
-}

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
    , codeColor : Element.Color
    , leftIndent : Int
    , leftIndentation : Int
    , leftRightIndentation : Int
    }


type Display
    = DefaultDisplay
    | PhoneDisplay


defaultSettings : Settings
defaultSettings =
    makeSettings "" Nothing 1 600


makeSettings : String -> Maybe String -> Float -> Int -> Settings
makeSettings id selectedSlug scale windowWidth =
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
    , codeColor = Element.rgb255 0 0 210
    , leftIndent = 18
    , leftIndentation = 18
    , leftRightIndentation = 18
    }


codeColor : Element.Color
codeColor =
    Element.rgb255 0 0 210


windowWidthScale =
    0.3


maxHeadingFontSize : Float
maxHeadingFontSize =
    32


topMarginForChildren =
    6


leftIndentation : Element.Attribute msg
leftIndentation =
    Element.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


wideLeftIndentation : Element.Attribute msg
wideLeftIndentation =
    Element.paddingEach { left = 54, right = 0, top = 0, bottom = 0 }


leftRightIndentation : Element.Attribute msg
leftRightIndentation =
    Element.paddingEach { left = 18, right = 8, top = 0, bottom = 0 }


redColor =
    Element.rgb 0.7 0 0


blueColor =
    Element.rgb 0 0 0.9

module Render.Settings exposing (Display(..), Settings, defaultSettings, makeSettings)

{-| The Settings record holds information needed to render a
parsed document. For example, the renderer needs to
know the width of the window in which the document
is to be displayed. This is given by the `.width` field.

@docs Display, Settings, defaultSettings, makeSettings

-}

import Element


{-| A record of nformation needed to render a document.
For instance, the`width`field defines the width of the
page in which the document is e
-}
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
    , wideLeftIndentation : Int
    , windowWidthScale : Float
    , maxHeadingFontSize : Float
    , redColor : Element.Color
    , topMarginForChildren : Int
    }


{-| -}
type Display
    = DefaultDisplay
    | PhoneDisplay


{-| -}
defaultSettings : Settings
defaultSettings =
    makeSettings "" Nothing 1 600


{-| -}
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
    , wideLeftIndentation = 54
    , windowWidthScale = 0.3
    , maxHeadingFontSize = 32
    , redColor = Element.rgb 0.7 0 0
    , topMarginForChildren = 6
    }

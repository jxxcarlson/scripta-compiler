module Scripta.API exposing
    ( compile, DisplaySettings
    , EditRecord, init, update, render, makeSettings, defaultSettings
    , fileNameForExport, prepareContentForExport, getImageUrls, Settings
    , Msg, SyntaxTree
    )

{-| Scripta.API provides the functions you will need for an application
that compiles source text in L0, microLaTeX, or XMarkdown to HTML.


# Simple compilation

@docs compile, DisplaySettings


## Example

`compile (displaySettings 0) "Pythagorean formula: $a^2 + b^2 = c^2$"` where
we define

    displaySettings : Int -> Scripta.API.DisplaySettings
    displaySettings counter =
        { windowWidth = 500
        , counter = counter
        , selectedId = "--"
        , selectedSlug = Nothing
        , scale = 0.8
        }

The counter field must be updated on each edit.
This is needed for the rendered text to be
properly updated. See the demo app in
folder `Example1`.


# Differential Compilation

Compilation can be sped up by keeping track of which blocks
of source text have changed and ony reparsing those blocks.
An `EditRecord` is used to keep track of what has changed
and what has not. In this setup, the `EditRecord` is
initialized with the source text using the `init` function.
On each document change it brought up to date by the
`update` function. The `render` function transforms
the current `EditRecord` into HTML.

@docs EditRecord, init, update, render, makeSettings, defaultSettings


# Export

The `export` and `fileNameForExport` are functions used to transform source
text in a given markup language to standard LaTeX. The transformed text
can be used to produce a PDF file or a tar files that contains both the
standare LaTeX source and a folder of images used in the documents.
See the code in modules `PDF` and `Main` of `Example2` for more details.
The Elm app sends data to `https://pdfServ.app`, a small server
(165 lines of Haskell code) where it is turned into a PDF file or
tar archive where it is then accessible by a GET request.
See [pdfServer2@Github](https://github.com/jxxcarlson/pdfServer2).

@docs fileNameForExport, prepareContentForExport, getImageUrls, Settings


# Compatibility

The PDF module in Example2 requires these.

@docs Msg, SyntaxTree

-}

import Compiler.ASTTools as ASTTools
import Compiler.AbstractDifferentialParser
import Compiler.Acc
import Compiler.DifferentialParser
import Dict exposing (Dict)
import Either
import Element exposing (..)
import Maybe.Extra
import Parser.Block exposing (ExpressionBlock(..))
import Parser.Forest exposing (Forest)
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Regex
import Render.Export.LaTeX
import Render.Markup
import Render.Msg exposing (MarkupMsg)
import Render.Settings
import Scripta.Language exposing (Language)
import Scripta.TOC
import Time
import Tree


{-| -}
type alias SyntaxTree =
    Forest ExpressionBlock


{-| -}
type alias Msg =
    MarkupMsg


{-| Compile source text in the given language using the given display settings.
-}
compile : DisplaySettings -> Language -> String -> List (Element Render.Msg.MarkupMsg)
compile displaySettings language sourceText =
    sourceText
        |> init Dict.empty language
        |> render displaySettings


{-|

  - windowWidth: set this to agree with the width
    of the window in pixels in which the rendered
    text is displayed.

  - counter: This is updated on each edit.
    For technical reasons (virtual Dom)
    this is needed for the text to display properly.

  - selectedId and selectedSlug: useful for interactive editing.

  - scale: a fudge factor

-}
type alias DisplaySettings =
    { windowWidth : Int
    , counter : Int
    , selectedId : String
    , selectedSlug : Maybe String
    , scale : Float
    }


{-| -}
init : Dict String String -> Language -> String -> Compiler.DifferentialParser.EditRecord
init importedFileDict language sourceText =
    Compiler.DifferentialParser.init importedFileDict language sourceText


{-| -}
update : EditRecord -> String -> EditRecord
update =
    Compiler.DifferentialParser.update


{-| -}
type alias EditRecord =
    Compiler.AbstractDifferentialParser.EditRecord (Tree.Tree PrimitiveBlock) (Tree.Tree ExpressionBlock) Compiler.Acc.Accumulator



-- VIEW


{-| -}
makeSettings : String -> Maybe String -> Float -> Int -> Render.Settings.Settings
makeSettings id selectedSlug scale width =
    { width = round (scale * toFloat width)
    , titleSize = 30
    , paragraphSpacing = 28
    , showTOC = True
    , showErrorMessages = False
    , selectedId = id
    , selectedSlug = selectedSlug
    , backgroundColor = Element.rgb 1 1 1
    , titlePrefix = ""
    , isStandaloneDocument = False
    }


renderSettings : DisplaySettings -> Render.Settings.Settings
renderSettings ds =
    Render.Settings.makeSettings ds.selectedId ds.selectedSlug ds.scale ds.windowWidth


{-| -}
render : DisplaySettings -> Compiler.DifferentialParser.EditRecord -> List (Element Render.Msg.MarkupMsg)
render displaySettings editRecord =
    let
        settings =
            renderSettings displaySettings
    in
    Scripta.TOC.view displaySettings.counter editRecord.accumulator (renderSettings displaySettings) editRecord.parsed :: renderBody displaySettings.counter settings editRecord


renderBody : Int -> Render.Settings.Settings -> Compiler.DifferentialParser.EditRecord -> List (Element Render.Msg.MarkupMsg)
renderBody count settings editRecord =
    Render.Markup.renderFromAST count editRecord.accumulator settings (body editRecord)



-- EXPORT


{-| Settings used by render
-}
type alias Settings =
    { paragraphSpacing : Int
    , selectedId : String
    , selectedSlug : Maybe String
    , showErrorMessages : Bool
    , showTOC : Bool
    , titleSize : Int
    , width : Int
    , backgroundColor : Element.Color
    , titlePrefix : String
    , isStandaloneDocument : Bool
    }


{-| -}
fileNameForExport : Forest ExpressionBlock -> String
fileNameForExport ast =
    ast
        |> ASTTools.title
        |> compressWhitespace
        |> String.replace " " "-"
        |> removeNonAlphaNum
        |> (\s -> s ++ ".tex")


{-| -}
prepareContentForExport : Time.Posix -> Settings -> Forest ExpressionBlock -> String
prepareContentForExport currentTime settings syntaxTree =
    let
        contentForExport : String
        contentForExport =
            Render.Export.LaTeX.export currentTime settings syntaxTree
    in
    contentForExport


{-| -}
getImageUrls : Forest ExpressionBlock -> List String
getImageUrls syntaxTree =
    syntaxTree
        |> List.map Tree.flatten
        |> List.concat
        |> List.map (\(ExpressionBlock { content }) -> Either.toList content)
        |> List.concat
        |> List.concat
        |> ASTTools.filterExpressionsOnName "image"
        |> List.map (ASTTools.getText >> Maybe.map String.trim)
        |> List.map (Maybe.andThen extractUrl)
        |> Maybe.Extra.values


extractUrl : String -> Maybe String
extractUrl str =
    str |> String.split " " |> List.head


compressWhitespace : String -> String
compressWhitespace string =
    userReplace "\\s\\s+" (\_ -> " ") string


removeNonAlphaNum : String -> String
removeNonAlphaNum string =
    userReplace "[^A-Za-z0-9\\-]" (\_ -> "") string


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


{-| -}
defaultSettings : Render.Settings.Settings
defaultSettings =
    Render.Settings.defaultSettings



-- PARSER INTERFACE


body : { a | parsed : Forest ExpressionBlock } -> Forest ExpressionBlock
body editRecord =
    editRecord.parsed
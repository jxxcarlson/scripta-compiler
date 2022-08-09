module Scripta.API exposing
    ( DisplaySettings
    , EditRecord
    , defaultSettings
    , export
    , fileNameForExport
    , init
    , makeSettings
    , render
    , update
    , compile
    )

{-|

Scripta.API provides most of the functions you will need for an application.

# Simple compilation

**Example.** `compile defaultSettings "Pythagorean formula: $a^2 + b^2 = c^2$"`

@docs  compile, DisplaySettings, defaultSettings, EditRecord, init, update,   makeSettings, render, export, fileNameForExport

-}


import Compiler.ASTTools
import Compiler.AbstractDifferentialParser
import Compiler.Acc
import Compiler.DifferentialParser
import Dict exposing (Dict)
import Element exposing (..)
import Parser.Block exposing (ExpressionBlock)
import Parser.Forest exposing (Forest)
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Regex
import Render.Export.LaTeX
import Render.Markup
import Render.Msg
import Render.Settings
import Scripta.Language exposing (Language)
import Scripta.TOC
import Time
import Tree




{-|

  Compile source text in the given language using the given display settings
-}
compile : DisplaySettings -> Language -> String -> List (Element Render.Msg.MarkupMsg)
compile displaySettings language sourceText =
    sourceText
      |> init Dict.empty language
      |> render displaySettings



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

{-| -}
type alias DisplaySettings =
    { windowWidth : Int
    , counter : Int
    , selectedId : String
    , selectedSlug : Maybe String
    , scale : Float
    }



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

{-| -}
export : Time.Posix -> Render.Settings.Settings -> Forest ExpressionBlock -> String
export =
    Render.Export.LaTeX.export

{-| -}
fileNameForExport : Forest ExpressionBlock -> String
fileNameForExport ast =
    ast
        |> Compiler.ASTTools.title
        |> compressWhitespace
        |> String.replace " " "-"
        |> removeNonAlphaNum
        |> (\s -> s ++ ".tex")


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

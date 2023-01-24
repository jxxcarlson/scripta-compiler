module MicroLaTeX.Parser.Transform exposing (handleImage, macroArg, pseudoBlockNamesWithContent, transform)

import Compiler.Util
import Dict exposing (Dict)
import MicroLaTeX.Parser.Line
import Parser as P
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)


pseudoBlockNamesWithContent =
    [ "title", "section", "subsection", "subsubsection", "subheading", "setcounter", "contents", "endnotes", "image" ]


sectionDict : Dict String String
sectionDict =
    Dict.fromList
        [ ( "section", "1" )
        , ( "subsection", "2" )
        , ( "subsubsection", "3" )
        , ( "subheading", "4" )
        ]


{-|

        The purpose of this function is to transform a primitive block
        like the one coming from a single-line paragraph with text
        "\section{Intro}" to an ordinary (blockType PBOrdinaryBlock)
        block with name "section", args ["1"], and content ["Introduction"].
        This is to coerce parsed MiniLaTeX source to our standard model.



        { indent = 0
        , lineNumber = 123
        , position = 4561
        , content = ["\section{Introduction}"]
        , name = Nothing
        , args = []
        , properties = Dict.empty
        , sourceText "\section{Introduction}"
        , blockType = PBParagraph
        }

        -->

        { indent = 0
        , lineNumber = 123
        , position = 4561
        , content = ["Introduction"]
        , name = Just "section"
        , args = ["1"]
        , properties = Dict.empty
        , sourceText "\section{Introduction}"
        , blockType = PBOrdinaryBlock
        }

-}
transform : PrimitiveBlock -> PrimitiveBlock
transform block =
    let
        normalizedContent =
            block.content
                |> List.map String.trimLeft
                |> normalize
    in
    case ( block.blockType, normalizedContent ) of
        ( PBVerbatim, _ ) ->
            block

        ( _, firstLine :: _ ) ->
            let
                name =
                    if String.left 1 firstLine == "\\" then
                        String.dropLeft 1 firstLine |> String.split "{" |> List.head |> Maybe.withDefault "---"

                    else
                        firstLine

                arg : Maybe String
                arg =
                    case P.run (Compiler.Util.macroValParserX name) firstLine of
                        Ok result ->
                            Just (result |> String.dropRight 1)

                        Err _ ->
                            Nothing
            in
            if List.member name pseudoBlockNamesWithContent then
                handlePseudoBlockWithContent name arg block

            else
                block

        _ ->
            block


macroArg : String -> String -> String
macroArg macroName str =
    String.replace ("\\" ++ macroName ++ "{") "" str |> String.dropRight 1


handlePseudoBlockWithContent : String -> Maybe String -> PrimitiveBlock -> PrimitiveBlock
handlePseudoBlockWithContent name maybeArg block =
    case maybeArg of
        Nothing ->
            { block
                | content = [] -- ("| section " ++ val) :: [ str ]
                , args = []
                , name = Just name
                , blockType = PBOrdinary
            }

        Just arg ->
            if name == "image" then
                handleImage block

            else
                case Dict.get name sectionDict of
                    Nothing ->
                        { block
                            | content = [ arg ] --("| " ++ macroName) :: [ str ]
                            , name = Just name
                            , args = [ arg ]
                            , blockType = PBOrdinary
                        }

                    Just val ->
                        { block
                            | content = [ arg ] -- ("| section " ++ val) :: [ str ]
                            , args = val :: []
                            , name = Just "section"
                            , blockType = PBOrdinary
                        }


handleImage block =
    let
        words =
            List.head block.content
                |> Maybe.withDefault "???"
                |> String.replace "\\image{" ""
                |> String.replace "}" ""
                |> String.words

        ( _, properties_ ) =
            Parser.PrimitiveBlock.argsAndProperties (List.drop 1 words)

        properties =
            properties_

        --case Dict.get "caption" properties_ of
        --    Just _ ->
        --        properties_
        --
        --    Nothing ->
        --        -- Make sure that there is a caption entry
        --        Dict.insert "caption" " " properties_
    in
    { block
        | blockType = PBVerbatim
        , args = []
        , name = Just "image"
        , properties = properties
        , content = List.take 1 words
    }


normalize : List String -> List String
normalize list =
    case list of
        "" :: rest ->
            rest

        _ ->
            list

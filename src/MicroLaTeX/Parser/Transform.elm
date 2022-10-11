module MicroLaTeX.Parser.Transform exposing (macroArg, transform)

import Compiler.Util
import Dict exposing (Dict)
import Parser as P
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)


pseudoBlockNamesWithContent =
    [ "title", "section", "subsection", "subsubsection", "subheading", "setcounter" ]


sectionDict : Dict String String
sectionDict =
    Dict.fromList
        [ ( "section", "1" )
        , ( "subsection", "2" )
        , ( "subsubsection", "3" )
        , ( "subheading", "4" )
        ]

{-|

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
    case normalizedContent of
        name_ :: _ ->
            let
                name =
                    (if String.left 1 name_ == "\\" then
                        String.dropLeft 1 name_ |> String.split "{" |> List.head |> Maybe.withDefault "---"

                     else
                        name_
                    )
                        |> String.trimRight

                macroExpr : Maybe String
                macroExpr =
                    case P.run (Compiler.Util.macroValParser name) name_ of
                        Ok result ->
                            Just result

                        Err _ ->
                            Nothing
            in
            if List.member name pseudoBlockNamesWithContent then
                handlePseudoBlockWithContent block name (macroArg name name_ |> Just)

            else
                block

        _ ->
            block


macroArg : String -> String -> String
macroArg macroName str =
    String.replace ("\\" ++ macroName ++ "{") "" str |> String.dropRight 1


handlePseudoBlockWithContent : PrimitiveBlock -> String -> Maybe String -> PrimitiveBlock
handlePseudoBlockWithContent block macroName macroExpr =
    case macroExpr of
        Nothing ->
            block

        Just str ->
            case Dict.get macroName sectionDict of
                Nothing ->
                    { block
                        | content = [ str ] --("| " ++ macroName) :: [ str ]
                        , name = Just macroName
                        , args = []
                        , blockType = PBOrdinary
                    }

                Just val ->
                    { block
                        | content = [ str ] -- ("| section " ++ val) :: [ str ]
                        , args = val :: []
                        , name = Just "section"
                        , blockType = PBOrdinary
                    }


normalize : List String -> List String
normalize list =
    case list of
        "" :: rest ->
            rest

        _ ->
            list

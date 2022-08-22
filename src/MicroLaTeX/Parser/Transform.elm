module MicroLaTeX.Parser.Transform exposing (transform)

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
                handlePseudoBlockWithContent block name macroExpr

            else
                block

        _ ->
            block


handlePseudoBlockWithContent block macroName macroExpr =
    case macroExpr of
        Nothing ->
            block

        Just str ->
            case Dict.get macroName sectionDict of
                Nothing ->
                    { block
                        | content = ("| " ++ macroName) :: [ str ]
                        , name = Just macroName
                        , args = []
                        , blockType = PBOrdinary
                    }

                Just val ->
                    { block
                        | content = ("| section " ++ val) :: [ str ]
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

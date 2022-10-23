module Parser.PrimitiveLaTeXBlock exposing (..)

import Dict exposing (Dict)
import Parser.Line as Line exposing (Line, PrimitiveBlockType(..), isEmpty, isNonEmptyBlank)
import Scripta.Language


type alias PrimitiveLaTeXBlock =
    { indent : Int
    , lineNumber : Int
    , position : Int
    , content : List String
    , name : Maybe String
    , args : List String
    , properties : Dict String String
    , sourceText : String
    , blockType : PrimitiveBlockType
    , status : Status
    }

type Status = Complete  | Incomplete

type alias State =
    { blocks : List PrimitiveLaTeXBlock
    , stack : List PrimitiveLaTeXBlock
    , currentBlock : Maybe PrimitiveLaTeXBlock
    , lines : List String
    , inBlock : Bool
    , indent : Int
    , level : Int
    , lineNumber : Int
    , position : Int
    , inVerbatim : Bool
    , isVerbatimLine : String -> Bool
    , count : Int
    , label : String
    }



parse_ :  (String -> Bool) -> List String -> List PrimitiveLaTeXBlock
parse_ isVerbatimLine lines =
    loop (init isVerbatimLine lines) nextStep
        |> List.map (\block -> finalize block)



-- TODO: think about the below


finalize : PrimitiveLaTeXBlock -> PrimitiveLaTeXBlock
finalize block =
    let
        content =
            List.reverse block.content

        sourceText =
            String.join "\n" content
    in
    { block | content = content, sourceText = sourceText }


{-|

    Recall: classify position lineNumber, where position
    is the position of the first charabcter in the source
    and lineNumber is the index of the current line in the source

-}
init :(String -> Bool) -> List String -> State
init isVerbatimLine lines =
    { blocks = []
    , stack = []
    , currentBlock = Nothing
    , lines = lines
    , indent = 0
    , level = -1
    , lineNumber = 0
    , inBlock = False
    , position = 0
    , inVerbatim = False
    , isVerbatimLine = isVerbatimLine
    , count = 0
    , label = "0, START"
    }


blockFromLine :  Line -> PrimitiveLaTeXBlock
blockFromLine ({ indent, lineNumber, position, prefix, content } as line) =
    { indent = indent
    , lineNumber = lineNumber
    , position = position
    , content = [ prefix ++ content ]
    , name = Nothing
    , args = []
    , properties = Dict.empty -- TODO complete this
    , sourceText = ""
    , blockType = Line.getBlockType Scripta.Language.MicroLaTeXLang line.content
    , status = Incomplete
    }
       --  |> Parser.PrimitiveLaTeXBlock.elaborate line


nextStep : State -> Step State (List PrimitiveLaTeXBlock)
nextStep state =
    case List.head state.lines of
        Nothing ->
            case state.currentBlock of
                Nothing ->
                    Done (List.reverse state.blocks)

                Just block_ ->
                    let
                        block =
                            { block_ | content = dropLast block_.content }

                        blocks =
                            if block.content == [ "" ] then
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                List.reverse state.blocks

                            else
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                List.reverse (block :: state.blocks)
                    in
                    Done blocks

        Just rawLine ->
            let
                newPosition =
                    if rawLine == "" then
                        state.position + 1

                    else
                        state.position + String.length rawLine + 1

                currentLine : Line
                currentLine =
                    -- TODO: the below is wrong
                    Line.classify state.position (state.lineNumber + 1) rawLine
            in
            case ( state.inBlock, isEmpty currentLine, isNonEmptyBlank currentLine ) of
                -- not in a block, pass over empty line
                ( False, True, _ ) ->
                    Loop (advance newPosition { state | label = "1, EMPTY" })

                -- not in a block, pass over blank, non-empty line
                ( False, False, True ) ->
                    Loop (advance newPosition { state | label = "2, PASS" })

                -- create a new block: we are not in a block, but
                -- the current line is nonempty and nonblank
                ( False, False, False ) ->
                    Loop (createBlock { state | label = "3, NEW" } currentLine)

                -- A nonempty line was encountered inside a block, so add it
                ( True, False, _ ) ->
                    Loop (addCurrentLine2 { state | label = "4, ADD" } currentLine)

                -- commit the current block: we are in a block and the
                -- current line is empty
                ( True, True, _ ) ->
                    Loop (commitBlock { state | label = "5, COMMIT" } currentLine)


advance : Int -> State -> State
advance newPosition state =
    { state
        | lines = List.drop 1 state.lines
        , lineNumber = state.lineNumber + 1
        , position = newPosition
        , count = state.count + 1
    }


addCurrentLine2 : State -> Line -> State
addCurrentLine2 state currentLine =
    case state.currentBlock of
        Nothing ->
            { state | lines = List.drop 1 state.lines }

        Just block ->
            { state
                | lines = List.drop 1 state.lines
                , lineNumber = state.lineNumber + 1
                , position = state.position + String.length currentLine.content
                , count = state.count + 1
                , currentBlock =
                    Just (addCurrentLine_ currentLine block)
            }



addCurrentLine_ : Line -> PrimitiveLaTeXBlock -> PrimitiveLaTeXBlock
addCurrentLine_ ({ prefix, content } as line) block =
    if block.blockType == PBVerbatim then
        if block.name == Just "math" then
            { block | content = line.content :: block.content, sourceText = block.sourceText ++ "\n" ++ prefix ++ content }

        else
            { block | content = (line.prefix ++ line.content) :: block.content, sourceText = block.sourceText ++ "\n" ++ prefix ++ content }

    else
        { block | content = line.content :: block.content, sourceText = block.sourceText ++ "\n" ++ prefix ++ content }


commitBlock : State -> Line -> State
commitBlock state currentLine =
    case state.currentBlock of
        Nothing ->
            { state
                | lines = List.drop 1 state.lines
                , indent = currentLine.indent
            }

        Just block_ ->
            let
                block =
                    case block_.blockType of
                        PBParagraph ->
                            block_

                        PBOrdinary ->
                            { block_ | content = dropLast block_.content }

                        PBVerbatim ->
                            { block_ | content = dropLast block_.content }

                ( currentBlock, newBlocks ) =
                    if block.content == [ "" ] then
                        ( Nothing, state.blocks )

                    else
                        ( Just (blockFromLine currentLine), block :: state.blocks )
            in
            { state
                | lines = List.drop 1 state.lines
                , lineNumber = state.lineNumber + 1
                , position = state.position + String.length currentLine.content
                , count = state.count + 1
                , blocks = newBlocks
                , inBlock = False
                , inVerbatim = state.isVerbatimLine currentLine.content
                , currentBlock = currentBlock
            }


createBlock : State -> Line -> State
createBlock state currentLine =
    let
        blocks =
            case state.currentBlock of
                Nothing ->
                    state.blocks

                -- When creating a new block push the current block onto state.blocks
                -- only if its content is nontrivial (not == [""])
                Just block ->
                    if block.content == [ "" ] then
                        state.blocks

                    else
                        block :: state.blocks

        newBlock =
             (blockFromLine currentLine)
    in
    { state
        | lines = List.drop 1 state.lines
        , stack = newBlock :: state.stack
        , lineNumber = state.lineNumber + 1
        , position = state.position + String.length currentLine.content
        , count = state.count + 1
        , indent = currentLine.indent
        , inBlock = True
        , blocks = blocks
    }
--- HELPERS

type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b

dropLast : List a -> List a
dropLast list =
    List.take (List.length list - 1) list

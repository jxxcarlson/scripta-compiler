module Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parse_)

import Dict exposing (Dict)
import List.Extra
import MicroLaTeX.Parser.ClassifyBlock as ClassifyBlock exposing (Classification(..))
import Parser.Line as Line exposing (Line, PrimitiveBlockType(..))
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


type Status
    = Complete
    | Incomplete


type alias State =
    { blocks : List PrimitiveLaTeXBlock
    , stack : List PrimitiveLaTeXBlock
    , labelStack : List ( ClassifyBlock.Classification, Int )
    , currentBlock : Maybe PrimitiveLaTeXBlock
    , lines : List String
    , firstBlockLine : Int
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


parse_ : (String -> Bool) -> List String -> List PrimitiveLaTeXBlock
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
init : (String -> Bool) -> List String -> State
init isVerbatimLine lines =
    { blocks = []
    , stack = []
    , labelStack = []
    , currentBlock = Nothing
    , lines = lines
    , firstBlockLine = 0
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


blockFromLine : Line -> PrimitiveLaTeXBlock
blockFromLine ({ indent, lineNumber, position, prefix, content } as line) =
    { indent = indent
    , lineNumber = lineNumber
    , position = position
    , content = []
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
            case ClassifyBlock.classify currentLine.content of
                CBeginBlock label ->
                    if state.inBlock then
                        Loop (addLine currentLine state)

                    else
                        Loop (state |> endBlock (CBeginBlock label) currentLine |> beginBlock (CBeginBlock label) currentLine)

                CEndBlock label ->
                    if state.inBlock then
                        Loop (state |> endBlock (CBeginBlock label) currentLine)

                    else
                        Loop (state |> endBlock (CBeginBlock label) currentLine |> transfer)

                CSpecialBlock label ->
                    Done []

                CMathBlockDelim ->
                    Done []

                CVerbatimBlockDelim ->
                    Done []

                CPlainText ->
                    if state.inBlock then
                        Loop (state |> addLine currentLine)

                    else if isEmpty currentLine then
                        Loop (handleBlank state)

                    else
                        Loop (beginBlock CPlainText currentLine state)

                CEmpty ->
                    Done []



-- HANDLERS


beginBlock : Classification -> Line -> State -> State
beginBlock classifier line state =
    let
        level =
            state.level + 1

        newBlock =
            blockFromLine line
    in
    { state
        | firstBlockLine = line.lineNumber
        , indent = line.indent
        , level = level
        , labelStack = ( classifier, level ) :: state.labelStack
        , stack = newBlock :: state.stack
    }


endBlock : Classification -> Line -> State -> State
endBlock classifier line state =
    if Just ( classifier, state.level ) == List.head state.labelStack then
        case List.Extra.uncons state.stack of
            Nothing ->
                state

            -- TODO: error state!
            Just ( block, stack_ ) ->
                let
                    newBlock =
                        { block | content = slice state.firstBlockLine line.lineNumber state.lines }
                in
                { state
                    | blocks = newBlock :: state.blocks
                    , stack = List.drop 1 state.stack
                    , labelStack = List.drop 1 state.labelStack
                    , level = state.level - 1
                }

    else
        state



-- TODO: error recovery


slice : Int -> Int -> List a -> List a
slice a b list =
    list |> List.take (b + 1) |> List.drop a


addLine : Line -> State -> State
addLine line state =
    state


handleBlank : State -> State
handleBlank state =
    state


transfer : State -> State
transfer state =
    state



--- STUFF


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
            blockFromLine currentLine
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


isEmpty : Line -> Bool
isEmpty line =
    String.replace " " "" line.content == ""

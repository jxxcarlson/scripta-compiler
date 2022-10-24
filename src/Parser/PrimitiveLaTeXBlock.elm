module Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parse_, print)

import Dict exposing (Dict)
import List.Extra
import MicroLaTeX.Parser.ClassifyBlock as ClassifyBlock exposing (Classification(..))
import Parser.Line as Line exposing (Line, PrimitiveBlockType(..))
import Scripta.Language


type alias PrimitiveLaTeXBlock =
    { indent : Int
    , lineNumber : Int
    , position : Int
    , level : Int
    , content : List String
    , name : Maybe String
    , args : List String
    , properties : Dict String String
    , sourceText : String
    , blockType : PrimitiveBlockType
    , status : Status
    }


print : PrimitiveLaTeXBlock -> String
print block =
    [ "BLOCK:"
    , "Type: " ++ Line.showBlockType block.blockType
    , "Name: " ++ showName block.name
    , "Level: " ++ String.fromInt block.level
    , "Status: " ++ showStatus block.status
    , "Line number: " ++ String.fromInt block.lineNumber
    , "Content:"
    , block.content |> List.indexedMap (\k s -> String.padLeft 3 ' ' (String.fromInt (k + 1 + block.lineNumber)) ++ ": " ++ s) |> String.join "\n"
    ]
        |> String.join "\n"


showName : Maybe String -> String
showName mstr =
    case mstr of
        Nothing ->
            "(anon)"

        Just name ->
            name


type Status
    = Complete
    | Incomplete


showStatus : Status -> String
showStatus status =
    case status of
        Complete ->
            "Complete"

        Incomplete ->
            "Incomplete"


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


blockFromLine : Int -> Line -> PrimitiveLaTeXBlock
blockFromLine level ({ indent, lineNumber, position, prefix, content } as line) =
    let
        classifier =
            ClassifyBlock.classify line.content

        ( blockType, label ) =
            getBlockTypeAndLabel line.content
    in
    { indent = indent
    , lineNumber = lineNumber
    , position = position
    , content = []
    , level = level
    , name = label
    , args = []
    , properties = Dict.empty -- TODO complete this
    , sourceText = ""
    , blockType = blockType
    , status = Incomplete
    }


getBlockTypeAndLabel : String -> ( PrimitiveBlockType, Maybe String )
getBlockTypeAndLabel str =
    case ClassifyBlock.classify str of
        CBeginBlock label ->
            ( PBOrdinary, Just label )

        _ ->
            ( PBParagraph, Nothing )



--  |> Parser.PrimitiveLaTeXBlock.elaborate line


nextStep : State -> Step State (List PrimitiveLaTeXBlock)
nextStep state =
    case List.Extra.getAt state.lineNumber state.lines of
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
                    Line.classify newPosition state.lineNumber rawLine
            in
            case ClassifyBlock.classify currentLine.content of
                CBeginBlock label ->
                    if state.level > -1 then
                        Loop (addLine currentLine state)

                    else
                        Loop (state |> endBlock (CBeginBlock label) currentLine |> beginBlock (CBeginBlock label) currentLine)

                -- Loop (state |> beginBlock (CBeginBlock label) currentLine)
                CEndBlock label ->
                    if state.level > -1 then
                        Loop (state |> endBlock (CBeginBlock label) currentLine)

                    else
                        Loop (state |> endBlock (CBeginBlock label) currentLine |> transfer)

                CSpecialBlock label ->
                    let
                        _ =
                            Debug.log "CSpecialBlock" state.count
                    in
                    Done []

                CMathBlockDelim ->
                    let
                        _ =
                            Debug.log "CMathBlockDelim" state.count
                    in
                    Done []

                CVerbatimBlockDelim ->
                    let
                        _ =
                            Debug.log "CVerbatimBlockDelim" state.count
                    in
                    Done []

                CPlainText ->
                    let
                        _ =
                            Debug.log "CPLainText" ( state.level, state.count, currentLine )
                    in
                    if state.level > -1 then
                        let
                            _ =
                                Debug.log "addLine" currentLine
                        in
                        Loop (state |> addLine currentLine)

                    else if isEmpty currentLine then
                        Loop (handleBlank state)

                    else
                        Loop (beginBlock CPlainText currentLine state)

                CEmpty ->
                    let
                        _ =
                            Debug.log "CEmpty" ( state.lineNumber, state.count )
                    in
                    Loop { state | lineNumber = state.lineNumber + 1 }



-- HANDLERS


beginBlock : Classification -> Line -> State -> State
beginBlock classifier line state =
    let
        _ =
            Debug.log "beginBlock" ( level, state.count, line )

        level =
            state.level + 1

        newBlock =
            blockFromLine level line
    in
    { state
        | lineNumber = line.lineNumber --- state.lineNumber -- + 1
        , firstBlockLine = line.lineNumber
        , indent = line.indent
        , level = level
        , labelStack = ( classifier, level ) :: state.labelStack |> Debug.log "beginBlock, labelStack"
        , stack = newBlock :: state.stack
        , count = state.count + 1
    }


endBlock : Classification -> Line -> State -> State
endBlock classifier line state =
    let
        _ =
            Debug.log "endBlock" ( state.level, state.count, line )
    in
    if Just ( classifier, state.level ) == List.head state.labelStack then
        case List.Extra.uncons state.stack of
            Nothing ->
                { state | lineNumber = state.lineNumber + 1 }

            -- TODO: error state!
            Just ( block, stack_ ) ->
                let
                    content =
                        -- ignore \begin{foo} and \end{foo}:
                        slice (state.firstBlockLine + 1) (line.lineNumber - 1) state.lines |> List.reverse

                    newBlock =
                        { block | content = content, status = Complete }

                    _ =
                        Debug.log "endBlock, newBlock.content" content
                in
                { state
                    | lineNumber = line.lineNumber -- state.lineNumber + 1
                    , blocks = newBlock :: state.blocks |> Debug.log "endBlock, blocks"
                    , stack = List.drop 1 state.stack
                    , labelStack = List.drop 1 state.labelStack |> Debug.log "endBlock, labelStack"
                    , level = state.level - 1 |> Debug.log "endBlock, finalLevel"
                    , count = state.count + 1
                }

    else
        { state | lineNumber = state.lineNumber + 1, count = state.count + 1 }



-- TODO: error recovery


slice : Int -> Int -> List a -> List a
slice a b list =
    list |> List.take (b + 1) |> List.drop a


addLine : Line -> State -> State
addLine line state =
    { state | lineNumber = state.lineNumber + 1, count = state.count + 1 }


handleBlank : State -> State
handleBlank state =
    let
        _ =
            Debug.log "handleBlank" ( state.level, state.count, state.lineNumber )
    in
    { state | lineNumber = state.lineNumber + 1, count = state.count + 1 }


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
                        ( Just (blockFromLine state.level currentLine), block :: state.blocks )
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
            blockFromLine state.level currentLine
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

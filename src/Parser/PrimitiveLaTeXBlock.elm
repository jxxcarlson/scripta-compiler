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


type alias State =
    { blocks : List PrimitiveLaTeXBlock
    , stack : List PrimitiveLaTeXBlock
    , labelStack : List Label
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


type alias Label =
    { classification : ClassifyBlock.Classification, level : Int, status : Status, lineNumber : Int }


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
    = Finished
    | Started
    | Filled


showStatus : Status -> String
showStatus status =
    case status of
        Finished ->
            "Finished"

        Started ->
            "Started"

        Filled ->
            "Filled"


parse_ : (String -> Bool) -> List String -> { blocks : List PrimitiveLaTeXBlock, stack : List PrimitiveLaTeXBlock }
parse_ isVerbatimLine lines =
    loop (init isVerbatimLine lines) nextStep



-- |> List.map (\block -> finalize block)
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
    , lineNumber = -1
    , inBlock = False
    , position = 0
    , inVerbatim = False
    , isVerbatimLine = isVerbatimLine
    , count = -1
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
    , status = Started
    }


getBlockTypeAndLabel : String -> ( PrimitiveBlockType, Maybe String )
getBlockTypeAndLabel str =
    case ClassifyBlock.classify str of
        CBeginBlock label ->
            ( PBOrdinary, Just label )

        _ ->
            ( PBParagraph, Nothing )



--  |> Parser.PrimitiveLaTeXBlock.elaborate line


nextStep : State -> Step State { blocks : List PrimitiveLaTeXBlock, stack : List PrimitiveLaTeXBlock }
nextStep state_ =
    let
        state =
            { state_ | lineNumber = state_.lineNumber + 1, count = state_.count + 1 }
    in
    case List.Extra.getAt state.lineNumber state.lines of
        Nothing ->
            Done { blocks = List.reverse state.blocks, stack = state.stack }

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
                    Loop (state |> beginBlock (CBeginBlock label) currentLine)

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
                    Done { blocks = [], stack = [] }

                CMathBlockDelim ->
                    let
                        _ =
                            Debug.log "CMathBlockDelim" state.count
                    in
                    Done { blocks = [], stack = [] }

                CVerbatimBlockDelim ->
                    let
                        _ =
                            Debug.log "CVerbatimBlockDelim" state.count
                    in
                    Done { blocks = [], stack = [] }

                CPlainText ->
                    if (List.head state.labelStack |> Maybe.map .status) == Just Filled || state.labelStack == [] then
                        Loop (beginBlock CPlainText currentLine state)

                    else
                        Loop (state |> addLine currentLine)

                CEmpty ->
                    case List.head state.labelStack |> Maybe.map .classification of
                        Just CPlainText ->
                            Loop <| endBlock CPlainText currentLine state

                        _ ->
                            Loop state



-- HANDLERS


beginBlock : Classification -> Line -> State -> State
beginBlock classifier line state =
    case List.Extra.uncons state.stack of
        Nothing ->
            beginBlock_ classifier line state

        Just ( _, _ ) ->
            beginBlock_ classifier line { state | stack = fillBlockOnStack state }


{-| update stack
-}
fillBlockOnStack : State -> List PrimitiveLaTeXBlock
fillBlockOnStack state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state.stack

        Just ( block, rest ) ->
            if (List.head state.labelStack |> Maybe.map .status) == Just Filled then
                state.stack

            else
                let
                    firstBlockLine =
                        List.head state.labelStack |> Maybe.map .lineNumber |> Maybe.withDefault 0

                    newBlock =
                        { block | status = Filled, content = slice (firstBlockLine + 1) (state.lineNumber - 1) state.lines }
                in
                newBlock :: rest


beginBlock_ : Classification -> Line -> State -> State
beginBlock_ classifier line state =
    let
        level =
            state.level + 1

        newBlock =
            blockFromLine level line

        labelStack =
            case List.Extra.uncons state.labelStack of
                Nothing ->
                    state.labelStack

                Just ( label, rest_ ) ->
                    { label | status = Filled } :: rest_
    in
    { state
        | lineNumber = line.lineNumber
        , firstBlockLine = line.lineNumber
        , indent = line.indent
        , level = level
        , labelStack = { classification = classifier, level = level, status = Started, lineNumber = line.lineNumber } :: labelStack
        , stack = newBlock :: state.stack
    }


endBlock : Classification -> Line -> State -> State
endBlock classifier line state =
    let
        labelHead : Maybe Label
        labelHead =
            List.head state.labelStack
    in
    if Just classifier == Maybe.map .classification labelHead && Just state.level == Maybe.map .level labelHead then
        case List.Extra.uncons state.stack of
            Nothing ->
                state

            -- TODO: error state!
            Just ( block, stack_ ) ->
                let
                    content =
                        case classifier of
                            CPlainText ->
                                slice state.firstBlockLine (line.lineNumber - 1) state.lines

                            _ ->
                                slice (state.firstBlockLine + 1) (line.lineNumber - 1) state.lines |> List.reverse

                    newBlock =
                        { block | content = content, status = Finished }
                in
                { state
                    | blocks = newBlock :: state.blocks
                    , stack = List.drop 1 (fillBlockOnStack state)
                    , labelStack = List.drop 1 state.labelStack
                    , level = state.level - 1
                }

    else
        case List.Extra.uncons state.stack of
            Nothing ->
                state

            -- TODO: ???
            Just ( block, rest ) ->
                case List.Extra.uncons state.labelStack of
                    Nothing ->
                        state

                    -- TODO: ???
                    Just ( label, _ ) ->
                        let
                            newBlock =
                                { block | content = slice label.lineNumber (line.lineNumber - 1) state.lines, status = Finished }
                        in
                        { state | blocks = newBlock :: state.blocks, stack = rest, labelStack = List.drop 1 state.labelStack }
                            |> finishBlock


finishBlock : State -> State
finishBlock state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( block, rest ) ->
            let
                newBlock =
                    { block | status = Finished }
            in
            { state
                | blocks = newBlock :: state.blocks
                , stack = List.drop 1 state.stack
                , labelStack = List.drop 1 state.labelStack
            }



-- TODO: error recovery


slice : Int -> Int -> List a -> List a
slice a b list =
    list |> List.take (b + 1) |> List.drop a


addLine : Line -> State -> State
addLine line state =
    state


handleBlank : State -> State
handleBlank state =
    let
        _ =
            Debug.log "handleBlank" ( state.level, state.count, state.lineNumber )
    in
    state


transfer : State -> State
transfer state =
    state



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

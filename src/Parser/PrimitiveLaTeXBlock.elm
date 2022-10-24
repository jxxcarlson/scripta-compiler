module Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parse, print)

{-|

    The 'parser' function transforms a list of strings into a list of primitive blocks
    for LaTeX, making use of an error recovery stategy in the case of syntax errors,
    e.g., unterminated blocks.

    The strategy is to examine each line in turn, building up a stack of blocks,
    moving them from the stack to the block list as blocks are closed, i.e.,
    are found to be properly terminated.  If the stack is nonempty after all
    blocks have been consumed, we know that there is a syntax error, and
    so an error recovery procedure is invoked.  Error recovery always
    terminates and provides an indication of the nature of the error.

-}

import Dict exposing (Dict)
import List.Extra
import MicroLaTeX.Parser.ClassifyBlock as ClassifyBlock exposing (Classification(..))
import Parser.Line as Line exposing (Line, PrimitiveBlockType(..))


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
    , error : Maybe PrimitiveBlockError
    }


type alias PrimitiveBlockError =
    { error : String }


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
    , count : Int
    , label : String
    }


type alias Label =
    { classification : ClassifyBlock.Classification, level : Int, status : Status, lineNumber : Int }


type Status
    = Finished
    | Started
    | Filled


type alias ParserOutput =
    { blocks : List PrimitiveLaTeXBlock, stack : List PrimitiveLaTeXBlock }


parse : List String -> ParserOutput
parse lines =
    loop (init lines) nextStep |> recover


recover1 : State -> ParserOutput
recover1 state =
    { blocks = state.blocks, stack = state.stack }


recover : State -> ParserOutput
recover state =
    case List.Extra.uncons state.stack of
        Nothing ->
            { blocks = state.blocks, stack = state.stack }

        Just ( block, rest ) ->
            case List.Extra.uncons state.labelStack of
                Nothing ->
                    { blocks = state.blocks, stack = state.stack }

                Just ( topLabel, remainingLabels ) ->
                    let
                        firstLine =
                            topLabel.lineNumber

                        lastLine =
                            state.lineNumber

                        content =
                            case topLabel.status of
                                Filled ->
                                    block.content

                                _ ->
                                    slice (firstLine + 1) lastLine state.lines

                        newBlock =
                            { block
                                | content = content
                                , status = Finished
                                , error = Just { error = "missing end tag (" ++ (block.name |> Maybe.withDefault "(anon)") ++ ")" }
                            }
                    in
                    { blocks = newBlock :: state.blocks, stack = rest }


{-|

    Recall: classify position lineNumber, where position
    is the position of the first character in the source
    and lineNumber is the index of the current line in the source

-}
init : List String -> State
init lines =
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
    , count = -1
    , label = "0, START"
    }


blockFromLine : Int -> Line -> PrimitiveLaTeXBlock
blockFromLine level ({ indent, lineNumber, position, prefix, content } as line) =
    let
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
    , error = Nothing
    }


getBlockTypeAndLabel : String -> ( PrimitiveBlockType, Maybe String )
getBlockTypeAndLabel str =
    case ClassifyBlock.classify str of
        CBeginBlock label ->
            ( PBOrdinary, Just label )

        _ ->
            ( PBParagraph, Nothing )


nextStep : State -> Step State State
nextStep state_ =
    let
        _ =
            Debug.log ("Step " ++ String.fromInt state_.lineNumber) state_.labelStack

        _ =
            Debug.log ("Step " ++ String.fromInt state_.lineNumber)
                ( List.head state_.stack |> Maybe.andThen .name, List.head state_.stack |> Maybe.map .content )

        state =
            { state_ | lineNumber = state_.lineNumber + 1, count = state_.count + 1 }
    in
    case List.Extra.getAt state.lineNumber state.lines of
        Nothing ->
            Done state

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
                    Done state

                CMathBlockDelim ->
                    Done state

                CVerbatimBlockDelim ->
                    Done state

                CPlainText ->
                    if (List.head state.labelStack |> Maybe.map .status) == Just Filled || state.labelStack == [] then
                        Loop (beginBlock CPlainText currentLine state)

                    else
                        Loop state

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

        Just ( block, rest ) ->
            beginBlock_ classifier line { state | stack = fillBlockOnStack state }


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

            else if (List.head state.labelStack |> Maybe.map .status) == Just Started then
                let
                    firstBlockLine =
                        List.head state.labelStack |> Maybe.map .lineNumber |> Maybe.withDefault 0

                    newBlock =
                        { block | status = Filled, content = slice (firstBlockLine + 1) (state.lineNumber - 1) state.lines }
                in
                newBlock :: rest

            else
                state.stack


endBlock : Classification -> Line -> State -> State
endBlock classifier line state =
    let
        labelHead : Maybe Label
        labelHead =
            List.head state.labelStack
    in
    if Just classifier == Maybe.map .classification labelHead && Just state.level == Maybe.map .level labelHead then
        let
            _ =
                Debug.log "END BLOCK!" labelHead

            _ =
                Debug.log "STACK TOP" (List.head state.stack)
        in
        case List.Extra.uncons state.stack of
            Nothing ->
                -- TODO: error state!
                state

            Just ( block, rest ) ->
                if (labelHead |> Maybe.map .status) == Just Filled then
                    { state | blocks = { block | status = Finished } :: state.blocks, stack = rest }

                else
                    let
                        _ =
                            Debug.log "endBlock" 1

                        content =
                            case classifier of
                                CPlainText ->
                                    slice state.firstBlockLine (line.lineNumber - 1) state.lines |> List.reverse

                                _ ->
                                    slice (state.firstBlockLine + 1) (line.lineNumber - 1) state.lines |> List.reverse

                        newBlock =
                            { block | content = List.reverse content, status = Finished } |> Debug.log "endBlock 1, newBlock"
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
                -- TODO: ???
                state

            Just ( block, rest ) ->
                case List.Extra.uncons state.labelStack of
                    Nothing ->
                        -- TODO: ???
                        state

                    Just ( label, _ ) ->
                        let
                            _ =
                                Debug.log "endBlock" 2

                            error =
                                if label.classification == CPlainText then
                                    Nothing

                                else
                                    let
                                        classfication1 =
                                            "(" ++ ClassifyBlock.classificationString label.classification ++ ")"

                                        classification2 =
                                            "(" ++ ClassifyBlock.classificationString classifier ++ ")"
                                    in
                                    Just { error = "Missmatched tags: begin" ++ classfication1 ++ " ≠ end" ++ classification2 }

                            newBlock =
                                { block
                                    | content = slice label.lineNumber (line.lineNumber - 1) state.lines
                                    , status = Finished
                                    , error = error
                                }
                                    |> Debug.log "endBlock 2, newBlock"
                        in
                        { state | blocks = newBlock :: state.blocks, stack = rest, labelStack = List.drop 1 state.labelStack }
                            |> finishBlock


finishBlock : State -> State
finishBlock state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( block, _ ) ->
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


transfer : State -> State
transfer state =
    state



--- PRINT


{-| Used for debugging with CLI.ReadFile
-}
print : PrimitiveLaTeXBlock -> String
print block =
    [ "BLOCK:"
    , "Type: " ++ Line.showBlockType block.blockType
    , "Name: " ++ showName block.name
    , "Level: " ++ String.fromInt block.level
    , "Status: " ++ showStatus block.status
    , "Error: " ++ showError block.error
    , "Line number: " ++ String.fromInt block.lineNumber
    , "Content:"
    , block.content |> List.indexedMap (\k s -> String.padLeft 3 ' ' (String.fromInt (k + 1 + block.lineNumber)) ++ ": " ++ s) |> String.join "\n"
    ]
        |> String.join "\n"


showError : Maybe PrimitiveBlockError -> String
showError mError =
    case mError of
        Nothing ->
            "none"

        Just { error } ->
            error


showName : Maybe String -> String
showName mstr =
    case mstr of
        Nothing ->
            "(anon)"

        Just name ->
            name


showStatus : Status -> String
showStatus status =
    case status of
        Finished ->
            "Finished"

        Started ->
            "Started"

        Filled ->
            "Filled"



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

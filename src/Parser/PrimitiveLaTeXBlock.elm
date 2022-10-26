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
import MicroLaTeX.Parser.ClassifyBlock as ClassifyBlock exposing (Classification(..), LXSpecial(..))
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
    , holdingStack : List PrimitiveLaTeXBlock
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
    loop (init lines) nextStep |> recover |> finalize


finalize : State -> ParserOutput
finalize state =
    { blocks = state.blocks |> List.reverse, stack = state.stack }


{-|

    Recall: classify position lineNumber, where position
    is the position of the first character in the source
    and lineNumber is the index of the current line in the source

-}
init : List String -> State
init lines =
    { blocks = []
    , stack = []
    , holdingStack = []
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


nextStep : State -> Step State State
nextStep state_ =
    let
        state =
            { state_ | lineNumber = state_.lineNumber + 1, count = state_.count + 1 }
    in
    case List.Extra.getAt state.lineNumber state.lines of
        Nothing ->
            Done state

        Just rawLine ->
            let
                currentLine =
                    Line.classify (getPosition rawLine state) state.lineNumber rawLine
            in
            case ClassifyBlock.classify currentLine.content of
                CBeginBlock label ->
                    Loop (state |> beginBlock (CBeginBlock label) currentLine)

                CEndBlock label ->
                    endBlock label currentLine state

                CSpecialBlock label ->
                    Loop <| handleSpecial (CSpecialBlock label) currentLine state

                CMathBlockDelim ->
                    Loop (state |> handleMathBlock currentLine)

                CVerbatimBlockDelim ->
                    Loop (state |> handleVerbatimBlock currentLine)

                CPlainText ->
                    plainText state currentLine

                CEmpty ->
                    emptyLine currentLine state



-- HANDLERS


beginBlock : Classification -> Line -> State -> State
beginBlock classifier line state =
    let
        _ =
            Debug.log ((state.count |> String.fromInt) ++ ": beginBlock, classifier") classifier
    in
    case List.Extra.uncons state.stack of
        Nothing ->
            beginBlock_ classifier line state

        Just _ ->
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


handleSpecial : Classification -> Line -> State -> State
handleSpecial classifier line state =
    case List.Extra.uncons state.stack of
        Nothing ->
            handleSpecial_ classifier line state

        Just _ ->
            handleSpecial_ classifier line { state | stack = fillBlockOnStack state }


handleSpecial_ : Classification -> Line -> State -> State
handleSpecial_ classifier line state =
    let
        level =
            state.level + 1

        newBlock_ =
            blockFromLine level line

        newBlock =
            case classifier of
                CSpecialBlock LXItem ->
                    { newBlock_
                        | name = Just "item"
                        , properties = Dict.fromList [ ( "firstLine", String.replace "\\item " "" line.content ) ]
                    }

                CSpecialBlock LXNumbered ->
                    { newBlock_
                        | name = Just "numbered"
                        , properties = Dict.fromList [ ( "firstLine", String.replace "\\numbered " "" line.content ) ]
                    }

                CSpecialBlock (LXOrdinaryBlock name) ->
                    { newBlock_
                        | name = Just name
                        , blockType = PBOrdinary
                    }

                CSpecialBlock (LXVerbatimBlock name) ->
                    { newBlock_
                        | name = Just name
                        , blockType = PBVerbatim
                    }

                _ ->
                    newBlock_

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


endBlock label currentLine state =
    if state.level > -1 then
        Loop (state |> endBlock_ (CBeginBlock label) currentLine)

    else
        Loop (state |> endBlock_ (CBeginBlock label) currentLine |> transfer)


endBlock_ : Classification -> Line -> State -> State
endBlock_ classifier line state =
    case List.head state.labelStack of
        Nothing ->
            state

        Just label ->
            let
                _ =
                    Debug.log ((state.count |> String.fromInt) ++ ": (c1, c2)") ( classifier, label.classification )
            in
            if classifier == label.classification && state.level == label.level then
                endBlockOMatch (Just label) classifier line state

            else
                endBlockOnMismatch label classifier line state


endBlockOnMismatch : Label -> Classification -> Line -> State -> State
endBlockOnMismatch label_ classifier line state =
    let
        _ =
            Debug.log ((state.count |> String.fromInt) ++ ": endBlockOnMismatch (ml, cl, li)") ( label_, classifier, line )
    in
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
                        error =
                            getError label classifier

                        newBlock =
                            { block
                                | content = getContent label_.classification line state
                                , status = Finished
                                , error = error
                            }
                                |> Debug.log ((state.count |> String.fromInt) ++ ": endBlockOnMismatch, newBlock")
                    in
                    { state
                        | holdingStack = newBlock :: state.holdingStack |> Debug.log ((state.count |> String.fromInt) ++ ": endBlockOnMismatch, holdingStack")

                        -- blocks = newBlock :: state.blocks
                        , stack = rest
                        , labelStack = List.drop 1 state.labelStack
                    }
                        |> finishBlock
                        |> resolveIfStackEmpty


resolveIfStackEmpty : State -> State
resolveIfStackEmpty state =
    if state.stack == [] then
        { state | blocks = state.holdingStack ++ state.blocks, holdingStack = [] }

    else
        state


finishBlock : State -> State
finishBlock state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( block, _ ) ->
            let
                updatedBlock =
                    { block | status = Finished }
            in
            { state
                | blocks = updatedBlock :: state.blocks
                , stack = List.drop 1 state.stack
                , labelStack = List.drop 1 state.labelStack
            }


endBlockOMatch : Maybe Label -> Classification -> Line -> State -> State
endBlockOMatch labelHead classifier line state =
    let
        _ =
            Debug.log ((state.count |> String.fromInt) ++ ": endBlockOnMatch") ( classifier, line )
    in
    case List.Extra.uncons state.stack of
        Nothing ->
            -- TODO: error state!
            state

        Just ( block, rest ) ->
            if (labelHead |> Maybe.map .status) == Just Filled then
                { state | blocks = { block | status = Finished } :: state.blocks, stack = rest } |> resolveIfStackEmpty

            else
                let
                    newBlock =
                        newBlockWithError classifier (getContent classifier line state) block
                in
                { state
                    | holdingStack = newBlock :: state.holdingStack |> Debug.log ((state.count |> String.fromInt) ++ ": endBlockOnMatch, holdingStack")

                    -- blocks = newBlock :: state.blocks
                    , stack = List.drop 1 (fillBlockOnStack state)
                    , labelStack = List.drop 1 state.labelStack |> Debug.log ((state.count |> String.fromInt) ++ ": endBlockOnMatch, stack")
                    , level = state.level - 1
                }
                    |> resolveIfStackEmpty


getError label classifier =
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


getContent : Classification -> Line -> State -> List String
getContent classifier line state =
    let
        _ =
            Debug.log ((state.count |> String.fromInt) ++ ": getContent") ( classifier, line )
    in
    case classifier of
        CPlainText ->
            slice state.firstBlockLine (line.lineNumber - 1) state.lines |> List.reverse

        _ ->
            slice (state.firstBlockLine + 1) (line.lineNumber - 1) state.lines |> List.reverse


newBlockWithError : Classification -> List String -> PrimitiveLaTeXBlock -> PrimitiveLaTeXBlock
newBlockWithError classifier content block =
    case classifier of
        CMathBlockDelim ->
            { block
                | content = List.reverse content
                , status = Finished
                , error = Just { error = "Missing $$ at end" }
            }

        CVerbatimBlockDelim ->
            { block
                | content = List.reverse content
                , status = Finished
                , error = Just { error = "Missing ``` at end" }
            }

        _ ->
            { block | content = List.reverse content, status = Finished }


plainText state currentLine =
    if (List.head state.labelStack |> Maybe.map .status) == Just Filled || state.labelStack == [] then
        Loop (beginBlock CPlainText currentLine state)

    else
        Loop state


emptyLine currentLine state =
    case List.head state.labelStack |> Maybe.map .classification of
        Just CPlainText ->
            Loop <| endBlock_ CPlainText currentLine state

        Just CMathBlockDelim ->
            Loop <| endBlock_ CMathBlockDelim currentLine state

        Just (CSpecialBlock LXItem) ->
            Loop <| endBlock_ (CSpecialBlock LXItem) currentLine state

        Just (CSpecialBlock LXNumbered) ->
            Loop <| endBlock_ (CSpecialBlock LXNumbered) currentLine state

        Just (CSpecialBlock (LXOrdinaryBlock name)) ->
            Loop <| endBlock_ (CSpecialBlock (LXOrdinaryBlock name)) currentLine state

        Just (CSpecialBlock (LXVerbatimBlock name)) ->
            Loop <| endBlock_ (CSpecialBlock (LXVerbatimBlock name)) currentLine state

        _ ->
            Loop state


handleMathBlock line state =
    case List.head state.labelStack of
        Nothing ->
            { state
                | lineNumber = line.lineNumber
                , firstBlockLine = line.lineNumber
                , indent = line.indent
                , level = state.level + 1
                , labelStack = { classification = CMathBlockDelim, level = state.level + 1, status = Started, lineNumber = line.lineNumber } :: state.labelStack
                , stack = blockFromLine (state.level + 1) line :: state.stack
            }

        Just label ->
            case List.Extra.uncons state.stack of
                Nothing ->
                    state

                Just ( block, rest ) ->
                    case List.Extra.uncons state.labelStack of
                        Nothing ->
                            state

                        Just ( topLabel, otherLabels ) ->
                            let
                                newBlock =
                                    { block | content = slice (topLabel.lineNumber + 1) (state.lineNumber - 1) state.lines, status = Finished }
                            in
                            { state | blocks = newBlock :: state.blocks, labelStack = otherLabels, stack = rest, level = state.level - 1 }


handleVerbatimBlock line state =
    case List.head state.labelStack of
        Nothing ->
            { state
                | lineNumber = line.lineNumber
                , firstBlockLine = line.lineNumber
                , indent = line.indent
                , level = state.level + 1
                , labelStack = { classification = CVerbatimBlockDelim, level = state.level + 1, status = Started, lineNumber = line.lineNumber } :: state.labelStack
                , stack = blockFromLine (state.level + 1) line :: state.stack
            }

        Just label ->
            case List.Extra.uncons state.stack of
                Nothing ->
                    state

                Just ( block, rest ) ->
                    case List.Extra.uncons state.labelStack of
                        Nothing ->
                            state

                        Just ( topLabel, otherLabels ) ->
                            let
                                newBlock =
                                    { block | content = slice (topLabel.lineNumber + 1) (state.lineNumber - 1) state.lines, status = Finished }
                            in
                            { state | blocks = newBlock :: state.blocks, labelStack = otherLabels, stack = rest, level = state.level - 1 }



-- ERROR RECOVERY


recover : State -> State
recover state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( block, rest ) ->
            case List.Extra.uncons state.labelStack of
                Nothing ->
                    state

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
                                , error = missingTagError block
                            }
                    in
                    { state | holdingStack = newBlock :: state.holdingStack, stack = rest } |> resolveIfStackEmpty


missingTagError : { a | name : Maybe String } -> Maybe { error : String }
missingTagError block =
    case block.name of
        Just "item" ->
            Nothing

        Nothing ->
            Nothing

        _ ->
            let
                name =
                    case block.name of
                        Just "math" ->
                            "$$"

                        Just "code" ->
                            "```"

                        _ ->
                            block.name |> Maybe.withDefault "(anon)"
            in
            Just { error = "missing end tag (" ++ name ++ ")" }



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
    , "Properties: " ++ showProperties block.properties
    , "Error: " ++ showError block.error
    , "Line number: " ++ String.fromInt block.lineNumber
    , "Content:"
    , block.content |> List.indexedMap (\k s -> String.padLeft 3 ' ' (String.fromInt (k + 1 + block.lineNumber)) ++ ": " ++ s) |> String.join "\n"
    ]
        |> String.join "\n"


showProperties : Dict String String -> String
showProperties dict =
    dict |> Dict.toList |> List.map (\( k, v ) -> k ++ ": " ++ v) |> String.join ", "


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


{-| Construct a skeleton block given one line of text, .e.g.,

        \begin{equation}

-}
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

        CMathBlockDelim ->
            ( PBVerbatim, Just "math" )

        CVerbatimBlockDelim ->
            ( PBVerbatim, Just "code" )

        _ ->
            ( PBParagraph, Nothing )


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


getPosition : String -> State -> Int
getPosition rawLine state =
    if rawLine == "" then
        state.position + 1

    else
        state.position + String.length rawLine + 1

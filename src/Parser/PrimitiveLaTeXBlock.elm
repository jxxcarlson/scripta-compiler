module Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parse, parseLoop, print, printErr)

{-|

    The 'parser' function transforms a list of strings into a list of primitive blocks
    for LaTeX, making use of an error recovery strategy in the case of syntax errors,
    e.g., unterminated blocks.

    The strategy is to examine each line in turn, building up a stack of blocks,
    moving them from the stack to the block list as blocks are closed, i.e.,
    are found to be properly terminated.  If the stack is nonempty after all
    blocks have been consumed, we know that there is a syntax error, and
    so an error recovery procedure is invoked.  Error recovery always
    terminates and provides an indication of the nature of the error.

-}

import Compiler.Util
import Dict exposing (Dict)
import List.Extra
import MicroLaTeX.Parser.ClassifyBlock as ClassifyBlock exposing (Classification(..), LXSpecial(..))
import MicroLaTeX.Parser.Line
import Parser.Line as Line exposing (Line, PrimitiveBlockType(..))


type alias PrimitiveLaTeXBlock =
    { indent : Int
    , lineNumber : Int
    , position : Int
    , level : Int
    , content : List String
    , numberOfLines : Int
    , firstLine : String
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
    { committedBlocks : List PrimitiveLaTeXBlock
    , stack : List PrimitiveLaTeXBlock
    , holdingStack : List PrimitiveLaTeXBlock
    , labelStack : List Label
    , lines : List String
    , sourceText : String
    , firstBlockLine : Int
    , indent : Int
    , level : Int
    , lineNumber : Int
    , position : Int
    , blockClassification : Maybe Classification
    , count : Int
    , label : String
    }


type alias Label =
    { classification : ClassifyBlock.Classification
    , level : Int
    , status : Status
    , lineNumber : Int
    }


type Status
    = Finished
    | Started
    | Filled


type alias ParserOutput =
    { blocks : List PrimitiveLaTeXBlock, stack : List PrimitiveLaTeXBlock, holdingStack : List PrimitiveLaTeXBlock }


parse : List String -> List PrimitiveLaTeXBlock
parse lines =
    lines |> parseLoop |> .blocks


parseLoop : List String -> ParserOutput
parseLoop lines =
    loop (init lines) nextStep |> finalize


finalize : State -> ParserOutput
finalize state =
    { blocks = state.committedBlocks |> List.reverse, stack = state.stack, holdingStack = state.holdingStack }


{-|

    Recall: classify position lineNumber, where position
    is the position of the first character in the source
    and lineNumber is the index of the current line in the source

-}
init : List String -> State
init lines =
    { committedBlocks = []
    , stack = []
    , holdingStack = []
    , labelStack = []
    , lines = lines
    , sourceText = ""
    , firstBlockLine = 0
    , indent = 0
    , level = -1
    , lineNumber = -1
    , position = 0
    , blockClassification = Nothing
    , count = -1
    , label = "0, START"
    }


{-|

    This is the driver function for the parser's functional loop.

      - Increment state.lineNumber.

      - If the input (state.lines) has been consumed and
            - the stack is empty, return Done state
            - the stack is non empty, return recoverFromError state
      - Let the current raw line be the string at index state.lineNumber of state.lines.
      - Classify the raw line, a value of type Classification:

            type Classification
                = CBeginBlock String
                | CEndBlock String
                | CSpecialBlock LXSpecial
                | CMathBlockDelim
                | CVerbatimBlockDelim
                | CPlainText
                | CEmpty

      - Invoke a handler based on the classification that returns a value
        of type Step State State

-}
nextStep : State -> Step State State
nextStep state_ =
    let
        state =
            { state_ | lineNumber = state_.lineNumber + 1, count = state_.count + 1 }
    in
    case List.Extra.getAt state.lineNumber state.lines of
        Nothing ->
            -- no more input, stack empty,so end the parser loop
            if List.isEmpty state.stack then
                Done state

            else
                -- no more input, but stack is empty, so there is an error:
                -- invoke the error recovery procedure
                Loop (recoverFromError state)

        Just rawLine ->
            let
                currentLine =
                    Line.classify (getPosition rawLine state) state.lineNumber rawLine
            in
            case ClassifyBlock.classify currentLine.content of
                CBeginBlock label ->
                    if List.member (List.head state.labelStack |> Maybe.map .classification) [ Just <| CBeginBlock "code" ] then
                        Loop state

                    else
                        Loop (state |> dispatchBeginBlock (CBeginBlock label) currentLine)

                CEndBlock label ->
                    -- TODO: changed, review
                    if List.member (state.labelStack |> List.reverse |> List.head |> Maybe.map .classification) [ Just <| CBeginBlock "code" ] then
                        state |> endBlockOnMatch Nothing (CBeginBlock "code") currentLine |> Loop

                    else
                        endBlock (CEndBlock label) currentLine state

                CSpecialBlock label ->
                    -- TODO: review all the List.member clauses
                    if List.member (List.head state.labelStack |> Maybe.map .classification) [ Just <| CBeginBlock "code" ] then
                        Loop state

                    else
                        Loop <| handleSpecialBlock (CSpecialBlock label) currentLine state

                CMathBlockDelim ->
                    -- TODO: changed, review
                    -- Loop (state |> handleMathBlock currentLine)
                    case List.head state.labelStack of
                        Nothing ->
                            Loop (state |> dispatchBeginBlock CMathBlockDelim currentLine)

                        Just label ->
                            if List.member label.classification [ CBeginBlock "code" ] then
                                Loop state

                            else if label.classification == CMathBlockDelim then
                                state |> endBlockOnMatch (Just label) CMathBlockDelim currentLine |> Loop

                            else
                                Loop (state |> dispatchBeginBlock CMathBlockDelim currentLine)

                CVerbatimBlockDelim ->
                    Loop (state |> handleVerbatimBlock currentLine)

                CPlainText ->
                    plainText state currentLine

                CEmpty ->
                    emptyLine currentLine state



-- HANDLERS


dispatchBeginBlock : Classification -> Line -> State -> State
dispatchBeginBlock classifier line state =
    case List.Extra.uncons state.stack of
        -- stack is empty; begin block
        Nothing ->
            beginBlock classifier line state

        -- stack is not empty; change status of stack top if need be, then begin block
        Just ( block, rest ) ->
            beginBlock classifier line { state | stack = changeStatusOfStackTop block rest state }


{-|

    1. Modify the classifier if need be to account for verbatim blocks

    2. Increase the block level, then construct a new block based on the
       new level and the current line; push this block onto the stack.

    3. If the labelStack is nonempty, set the status of the top label to Filled.

    4. Update the state with the results of these computations.

-}
beginBlock : Classification -> Line -> State -> State
beginBlock classifier line state =
    let
        newBlockClassifier =
            case classifier of
                CBeginBlock name ->
                    if List.member name verbatimBlockNames then
                        Just classifier

                    else
                        Nothing

                _ ->
                    Nothing

        level =
            state.level + 1

        newBlock =
            blockFromLine level line |> elaborate line

        labelStack =
            case List.Extra.uncons state.labelStack of
                Nothing ->
                    state.labelStack

                Just ( label, rest_ ) ->
                    { label | status = Filled } :: rest_
    in
    { state
        | lineNumber = line.lineNumber
        , blockClassification = newBlockClassifier
        , firstBlockLine = line.lineNumber
        , indent = line.indent
        , level = level
        , labelStack = { classification = classifier, level = level, status = Started, lineNumber = line.lineNumber } :: labelStack
        , stack = newBlock :: state.stack
    }


handleSpecialBlock : Classification -> Line -> State -> State
handleSpecialBlock classifier line state =
    case List.Extra.uncons state.stack of
        Nothing ->
            handleSpecial_ classifier line state

        Just ( block, rest ) ->
            handleSpecial_ classifier line { state | stack = changeStatusOfStackTop block rest state }


handleSpecial_ : Classification -> Line -> State -> State
handleSpecial_ classifier line state =
    let
        level =
            state.level + 1

        newBlock_ =
            blockFromLine level line |> elaborate line

        newBlock =
            case classifier of
                CSpecialBlock LXItem ->
                    { newBlock_
                        | name = Just "item"
                        , blockType = PBOrdinary
                        , properties = Dict.fromList [ ( "firstLine", String.replace "\\item" "" line.content ) ]
                    }

                CSpecialBlock LXNumbered ->
                    { newBlock_
                        | name = Just "numbered"
                        , blockType = PBOrdinary
                        , properties = Dict.fromList [ ( "firstLine", String.replace "\\numbered" "" line.content ) ]
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


{-|

    This function changes the status of the block on top of the
    stack to Filled if status = Started.

-}
changeStatusOfStackTop : PrimitiveLaTeXBlock -> List PrimitiveLaTeXBlock -> State -> List PrimitiveLaTeXBlock
changeStatusOfStackTop block rest state =
    if (List.head state.labelStack |> Maybe.map .status) == Just Filled then
        state.stack

    else if (List.head state.labelStack |> Maybe.map .status) == Just Started then
        let
            firstBlockLine =
                List.head state.labelStack |> Maybe.map .lineNumber |> Maybe.withDefault 0

            newBlock =
                let
                    content =
                        slice (firstBlockLine + 1) (state.lineNumber - 1) state.lines

                    numberOfLines =
                        List.length content
                in
                -- set the status to Filled and grab lines from state.lines to fill the content field of the block
                { block
                    | status = Filled
                    , content = slice (firstBlockLine + 1) (state.lineNumber - 1) state.lines
                    , numberOfLines = numberOfLines
                }
        in
        newBlock :: rest

    else
        state.stack


{-| We arrive here only from clause CEndBlock of function nextStep.
The classification is that of the current line, e.g. 'CEndBlock "theorem"'
-}
endBlock : Classification -> Line -> State -> Step State State
endBlock classification currentLine state =
    -- TODO: changed, review
    case List.head state.labelStack of
        Nothing ->
            Loop <| { state | level = state.level - 1 }

        Just label ->
            if ClassifyBlock.match label.classification classification && state.level == label.level then
                -- the current classification agrees with the one on top of the stack
                Loop <| endBlockOnMatch (Just label) classification currentLine { state | blockClassification = Nothing }

            else
                -- the current classification disagrees with the one on top of the stack
                Loop <| endBlockOnMismatch label classification currentLine { state | blockClassification = Nothing }


endBlockOnMismatch : Label -> Classification -> Line -> State -> State
endBlockOnMismatch label_ classifier line state =
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
                        name =
                            case block.name of
                                Nothing ->
                                    "--"

                                Just name_ ->
                                    if List.member name_ [ "math", "equation", "aligned" ] then
                                        "code"

                                    else
                                        name_

                        content =
                            --- TODO: WTF!?
                            if List.member name verbatimBlockNames then
                                getContent label_.classification line state |> List.reverse

                            else
                                getContent label_.classification line state |> List.reverse

                        newBlock =
                            { block
                                | name = Just name
                                , content = content
                                , numberOfLines = List.length content
                                , args =
                                    if List.member name [ "item", "numbered" ] then
                                        block.content

                                    else
                                        block.args
                                , status = Finished
                                , error =
                                    case ( label.classification, classifier ) of
                                        ( CBeginBlock a, CEndBlock b ) ->
                                            Just { error = "Mismatch: \\begin{" ++ a ++ "} ≠ \\end{" ++ b ++ "}" }

                                        ( CBeginBlock a, _ ) ->
                                            Just { error = "Missing \\end{" ++ a ++ "}" }

                                        _ ->
                                            -- TODO: Is this the right thing to do?
                                            Nothing
                            }
                                |> addSource line.content
                    in
                    { state
                        | holdingStack = newBlock :: state.holdingStack
                        , level = state.level - 1
                        , stack = rest
                        , labelStack = List.drop 1 state.labelStack
                    }
                        |> finishBlock line.content
                        |> resolveIfStackEmpty


resolveIfStackEmpty : State -> State
resolveIfStackEmpty state =
    if state.stack == [] then
        { state | committedBlocks = state.holdingStack ++ state.committedBlocks, holdingStack = [] }

    else
        state


finishBlock : String -> State -> State
finishBlock lastLine state =
    case List.Extra.uncons state.stack of
        Nothing ->
            state

        Just ( block, _ ) ->
            let
                updatedBlock =
                    { block | status = Finished, numberOfLines = List.length block.content } |> addSource lastLine
            in
            { state
                | committedBlocks = updatedBlock :: state.committedBlocks
                , stack = List.drop 1 state.stack
                , labelStack = List.drop 1 state.labelStack
            }


{-| Be sure to decrement level (both branches of if) when the end of a block is reached.
-}
endBlockOnMatch : Maybe Label -> Classification -> Line -> State -> State
endBlockOnMatch labelHead classifier line state =
    case List.Extra.uncons state.stack of
        Nothing ->
            -- TODO: error state!
            state

        Just ( block, rest ) ->
            if (labelHead |> Maybe.map .status) == Just Filled then
                { state | level = state.level - 1, committedBlocks = ({ block | status = Finished } |> addSource line.content) :: state.committedBlocks, stack = rest } |> resolveIfStackEmpty

            else
                let
                    newBlock =
                        if classifier == CSpecialBlock (LXVerbatimBlock "texComment") then
                            newBlockWithError classifier (getContent classifier line state ++ [ block.firstLine ]) block |> addSource line.content

                        else if List.member classifier (List.map CEndBlock verbatimBlockNames) then
                            let
                                sourceText =
                                    getSource line state
                            in
                            newBlockWithError classifier
                                (getContent classifier line state)
                                { block
                                    | numberOfLines = List.length block.content
                                    , sourceText = sourceText
                                }

                        else
                            newBlockWithOutError (getContent classifier line state) block |> addSource line.content
                in
                { state
                    | holdingStack = newBlock :: state.holdingStack

                    -- blocks = newBlock :: state.blocks
                    -- , stack = List.drop 1 (fillBlockOnStack state)
                    -- TODO. I think the change below is OK because (referring to line above),
                    -- when fillBlockOnStack is invoked, it uses the current state, hence
                    -- the values block and rest as defined in Just (block, rest) ->
                    , stack = List.drop 1 (changeStatusOfStackTop block rest state)
                    , labelStack = List.drop 1 state.labelStack
                    , level = state.level - 1
                }
                    |> resolveIfStackEmpty


addSource : String -> PrimitiveLaTeXBlock -> PrimitiveLaTeXBlock
addSource lastLine block =
    case block.name of
        Nothing ->
            { block | sourceText = String.join "\n" block.content, numberOfLines = List.length block.content }

        _ ->
            { block
                | sourceText = block.firstLine ++ "\n" ++ String.join "\n" block.content ++ "\n" ++ lastLine
                , numberOfLines = List.length block.content + 2
            }


getError label classifier =
    if label.classification == CPlainText then
        Nothing

    else if ClassifyBlock.classificationString classifier == "missing" then
        Just { error = "Missing end tag (" ++ ClassifyBlock.classificationString label.classification ++ ")" }

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
    case classifier of
        CPlainText ->
            slice state.firstBlockLine (line.lineNumber - 1) state.lines |> List.reverse

        CSpecialBlock LXItem ->
            slice state.firstBlockLine line.lineNumber state.lines
                |> List.reverse
                |> List.map (\line_ -> String.replace "\\item" "" line_ |> String.trim)

        CSpecialBlock LXNumbered ->
            slice state.firstBlockLine line.lineNumber state.lines
                |> List.reverse
                |> List.map (\line_ -> String.replace "\\numbered" "" line_ |> String.trim)

        CEndBlock _ ->
            -- TODO: is this robust?
            slice (state.firstBlockLine + 1) (line.lineNumber - 1) state.lines
                |> List.reverse

        _ ->
            slice (state.firstBlockLine + 1) (line.lineNumber - 1) state.lines |> List.reverse


getSource : Line -> State -> String
getSource line state =
    slice state.firstBlockLine line.lineNumber state.lines |> String.join "\n"


newBlockWithOutError : List String -> PrimitiveLaTeXBlock -> PrimitiveLaTeXBlock
newBlockWithOutError content block =
    { block
        | content = List.reverse content
        , status = Finished
    }


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

        CSpecialBlock LXItem ->
            { block
                | content = List.reverse content |> List.filter (\line_ -> line_ /= "")
                , properties = Dict.empty
                , status = Finished
            }

        CSpecialBlock LXNumbered ->
            { block
                | content = List.reverse content |> List.filter (\line_ -> line_ /= "")
                , properties = Dict.empty
                , status = Finished
            }

        _ ->
            { block | content = List.reverse content, status = Finished }


plainText state currentLine =
    if (List.head state.labelStack |> Maybe.map .status) == Just Filled || state.labelStack == [] then
        if String.left 1 currentLine.content == "%" then
            Loop (handleComment currentLine state)

        else
            Loop (dispatchBeginBlock CPlainText currentLine state)

    else
        Loop state


handleComment : Line -> State -> State
handleComment line state =
    let
        newBlock =
            blockFromLine 0 line |> (\b -> { b | numberOfLines = 1, name = Just "texComment", blockType = PBVerbatim })

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
        , level = 0
        , labelStack = { classification = CSpecialBlock (LXVerbatimBlock "texComment"), level = 0, status = Started, lineNumber = line.lineNumber } :: labelStack
        , stack = newBlock :: state.stack
    }


elaborate : Line -> PrimitiveLaTeXBlock -> PrimitiveLaTeXBlock
elaborate line pb =
    if pb.content == [ "" ] then
        pb

    else
        let
            ( name, args_ ) =
                MicroLaTeX.Parser.Line.getNameAndArgString line

            namedArgs =
                getKVData args_

            simpleArgs =
                case name of
                    Nothing ->
                        -- not a \begin{??} ... \end{??} block
                        getArgs args_

                    Just name_ ->
                        -- is a \begin{??} ... \end{??} block
                        let
                            prefix =
                                "\\begin{" ++ name_ ++ "}"

                            adjustedLine =
                                String.replace prefix "" line.content
                        in
                        Compiler.Util.getBracedItems adjustedLine

            properties =
                namedArgs |> prepareList |> prepareKVData

            content =
                if pb.blockType == PBVerbatim then
                    List.map String.trimLeft pb.content

                else
                    pb.content
        in
        { pb | content = content, name = name, args = simpleArgs, properties = properties }


{-| return all the comma-separated elements of the given Maybe String that contain
the character ':' and hence are potentially elements defining key:value pairs.

    > getKVData (Just "foo:bar, a:b:c, hoho")
    ["foo:bar","a:b:c"]

-}
getKVData : Maybe String -> List String
getKVData mstr =
    case mstr of
        Nothing ->
            []

        Just str ->
            let
                strs =
                    String.split ", " str |> List.map String.trim
            in
            List.filter (\t -> String.contains ":" t) strs


{-| return all the comma-separated elements of the given Maybe String that do not contain
the character ':' and hence are not elements defining key:value pairs.

        > getArgs (Just "foo:bar, a:b:c, hoho")
        ["hoho"]

-}
getArgs : Maybe String -> List String
getArgs mstr =
    case mstr of
        Nothing ->
            []

        Just str ->
            let
                strs =
                    String.split ", " str |> List.map String.trim
            in
            List.filter (\t -> not <| String.contains ":" t) strs



--case List.Extra.findIndex (\t -> String.contains ":" t) strs of
--    Nothing ->
--        strs
--
--    Just k ->
--        List.take k strs


explode : List String -> List (List String)
explode txt =
    List.map (String.split ":") txt


prepareList : List String -> List String
prepareList strs =
    strs |> explode |> List.map fix |> List.concat


fix : List String -> List String
fix strs =
    case strs of
        a :: b :: _ ->
            (a ++ ":") :: b :: []

        a :: [] ->
            a :: []

        [] ->
            []


prepareKVData : List String -> Dict String String
prepareKVData data_ =
    let
        initialState =
            { input = data_, kvList = [], currentKey = Nothing, currentValue = [], kvStatus = KVInKey }
    in
    loop initialState nextKVStep


type alias KVState =
    { input : List String
    , kvList : List ( String, List String )
    , currentKey : Maybe String
    , currentValue : List String
    , kvStatus : KVStatus
    }


type KVStatus
    = KVInKey
    | KVInValue


nextKVStep : KVState -> Step KVState (Dict String String)
nextKVStep state =
    case List.Extra.uncons <| state.input of
        Nothing ->
            let
                kvList_ =
                    case state.currentKey of
                        Nothing ->
                            state.kvList

                        Just key ->
                            ( key, state.currentValue )
                                :: state.kvList
                                |> List.map (\( k, v ) -> ( k, List.reverse v ))
            in
            Done (Dict.fromList (List.map (\( k, v ) -> ( k, String.join " " v )) kvList_))

        Just ( item, rest ) ->
            case state.kvStatus of
                KVInKey ->
                    if String.contains ":" item then
                        case state.currentKey of
                            Nothing ->
                                Loop { state | input = rest, currentKey = Just (String.dropRight 1 item), kvStatus = KVInValue }

                            Just key ->
                                Loop
                                    { input = rest
                                    , currentKey = Just (String.dropRight 1 item)
                                    , kvStatus = KVInValue
                                    , kvList = ( key, state.currentValue ) :: state.kvList
                                    , currentValue = []
                                    }

                    else
                        Loop { state | input = rest }

                KVInValue ->
                    if String.contains ":" item then
                        case state.currentKey of
                            Nothing ->
                                Loop
                                    { state
                                        | input = rest
                                        , currentKey = Just (String.dropRight 1 item)
                                        , currentValue = []
                                        , kvStatus = KVInValue
                                    }

                            Just key ->
                                Loop
                                    { state
                                        | input = rest
                                        , currentKey = Just (String.dropRight 1 item)
                                        , kvStatus = KVInValue
                                        , kvList = ( key, state.currentValue ) :: state.kvList
                                        , currentValue = []
                                    }

                    else
                        Loop { state | input = rest, currentValue = item :: state.currentValue }


emptyLine currentLine state =
    case List.head state.labelStack of
        Nothing ->
            -- { state | blockClassification = Nothing, level = state.level - 1 }
            Loop (resetLevelIfStackIsEmpty state)

        Just label ->
            case label.classification of
                CPlainText ->
                    endBlock CPlainText currentLine state

                CMathBlockDelim ->
                    Loop <| endBlockOnMismatch label CMathBlockDelim currentLine state

                CBeginBlock name ->
                    if List.member name [ "equation", "aligned" ] then
                        -- equation and aligned blocks are terminated if an empty line is encountered
                        Loop <| endBlockOnMismatch label (CBeginBlock name) currentLine state

                    else
                        -- if the top of the labelstack is  (CBeginBlock name) and empty line
                        -- is encounter, keep going to try to find (CEndBlock name)
                        Loop <| state

                CSpecialBlock LXPseudoBlock ->
                    endBlock (CSpecialBlock LXItem) currentLine state

                CSpecialBlock LXItem ->
                    endBlock (CSpecialBlock LXItem) currentLine state

                CSpecialBlock LXNumbered ->
                    endBlock (CSpecialBlock LXNumbered) currentLine state

                CSpecialBlock (LXOrdinaryBlock name) ->
                    endBlock (CSpecialBlock (LXOrdinaryBlock name)) currentLine state

                CSpecialBlock (LXVerbatimBlock name) ->
                    endBlock (CSpecialBlock (LXVerbatimBlock name)) currentLine state

                CEndBlock _ ->
                    Loop (resetLevelIfStackIsEmpty state)

                CVerbatimBlockDelim ->
                    Loop (resetLevelIfStackIsEmpty state)

                CEmpty ->
                    Loop (resetLevelIfStackIsEmpty state)


resetLevelIfStackIsEmpty : State -> State
resetLevelIfStackIsEmpty state =
    if List.isEmpty state.stack then
        { state | level = -1 }

    else
        state


handleMathBlock : Line -> State -> State
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
                                content =
                                    slice (topLabel.lineNumber + 1) (state.lineNumber - 1) state.lines

                                newBlock =
                                    { block | content = content, numberOfLines = List.length content, status = Finished } |> addSource "$$"
                            in
                            { state | committedBlocks = newBlock :: state.committedBlocks, labelStack = otherLabels, stack = rest, level = state.level - 1 }


handleVerbatimBlock line state =
    case List.head state.labelStack of
        Nothing ->
            { state
                | lineNumber = line.lineNumber
                , firstBlockLine = line.lineNumber
                , indent = line.indent
                , level = state.level + 1
                , labelStack = { classification = CVerbatimBlockDelim, level = state.level + 1, status = Started, lineNumber = line.lineNumber } :: state.labelStack
                , stack = (blockFromLine (state.level + 1) line |> elaborate line) :: state.stack
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
                                    { block | content = slice (topLabel.lineNumber + 1) (state.lineNumber - 1) state.lines, status = Finished } |> addSource line.content
                            in
                            { state | committedBlocks = newBlock :: state.committedBlocks, labelStack = otherLabels, stack = rest, level = state.level - 1 }



-- ERROR RECOVERY


recoverFromError : State -> State
recoverFromError state =
    case List.Extra.unconsLast state.stack of
        Nothing ->
            state

        Just ( block, _ ) ->
            case List.Extra.unconsLast state.labelStack of
                Nothing ->
                    state

                Just ( topLabel, _ ) ->
                    let
                        firstLineNumber =
                            topLabel.lineNumber

                        lastLineNumber =
                            state.lineNumber

                        provisionalContent =
                            case topLabel.status of
                                Filled ->
                                    -- the block is Filled, so its content is already set
                                    block.content

                                _ ->
                                    -- the block is not filled, so we grab it content from state.lines
                                    slice (firstLineNumber + 1) lastLineNumber state.lines

                        content =
                            -- remove blank lines
                            -- TODO: is this the right thing to do?
                            List.Extra.takeWhile (\item -> item /= "") provisionalContent

                        revisedContent =
                            -- drop the last line if it is "\\end{...}"
                            case List.Extra.last content of
                                Nothing ->
                                    content

                                Just str ->
                                    if String.left 4 str == "\\end" then
                                        Compiler.Util.dropLast content

                                    else
                                        content

                        lineNumber =
                            -- set the line number to be just past the current block
                            firstLineNumber + List.length content + 1

                        newBlock =
                            -- set the content of the block, declare it to be finished,
                            -- and add and error message
                            { block
                                | content = revisedContent
                                , status = Finished
                                , error = missingTagError block
                            }
                                |> addSource ""
                    in
                    { state
                        | committedBlocks = newBlock :: state.committedBlocks
                        , stack = []
                        , holdingStack = []
                        , labelStack = []
                        , lineNumber = lineNumber
                        , blockClassification = Nothing
                    }


missingTagError : { b | name : Maybe String } -> Maybe { error : String }
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
                            "code"

                        _ ->
                            block.name |> Maybe.withDefault "(anon)"
            in
            Just { error = "Missing \\end{" ++ name ++ "}" }


slice : Int -> Int -> List a -> List a
slice a b list =
    list |> List.take (b + 1) |> List.drop a


transfer : State -> State
transfer state =
    state



--- PRINT


printErr : PrimitiveLaTeXBlock -> String
printErr block =
    showError block.error


{-| Used for debugging with CLI.LXPB
-}
print : PrimitiveLaTeXBlock -> String
print block =
    [ "BLOCK:"
    , "Type: " ++ Line.showBlockType block.blockType
    , "Name: " ++ showName block.name
    , "Level: " ++ String.fromInt block.level
    , "Status: " ++ showStatus block.status
    , "Args: " ++ showArgs block.args
    , "Properties: " ++ showProperties block.properties
    , "Error: " ++ showError block.error
    , "Line number: " ++ String.fromInt block.lineNumber
    , "Number of lines: " ++ String.fromInt block.numberOfLines
    , "Content:"
    , block.content |> List.indexedMap (\k s -> String.padLeft 3 ' ' (String.fromInt (k + 1 + block.lineNumber)) ++ ": " ++ s) |> String.join "\n"
    , "Source text:\n" ++ block.sourceText
    ]
        |> String.join "\n"


showProperties : Dict String String -> String
showProperties dict =
    dict |> Dict.toList |> List.map (\( k, v ) -> k ++ ": " ++ v) |> String.join ", "


showArgs : List String -> String
showArgs args =
    args |> String.join ", "


showError : Maybe PrimitiveBlockError -> String
showError mError =
    case mError of
        Nothing ->
            ""

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
    , numberOfLines = 1
    , firstLine = line.content
    , level = level
    , name = label
    , args = []
    , properties = Dict.empty -- TODO complete this
    , sourceText = ""
    , blockType = blockType
    , status = Started
    , error = Nothing
    }


verbatimBlockNames =
    [ "equation"
    , "aligned"
    , "math"
    , "code"
    , "verbatim"
    , "verse"
    , "mathmacros"
    , "textmacros"
    , "tabular"
    , "hide"
    , "docinfo"
    , "datatable"
    , "chart"
    , "svg"
    , "quiver"
    , "image"
    , "tikz"
    , "load-files"
    , "include"
    , "iframe"
    ]


getBlockTypeAndLabel : String -> ( PrimitiveBlockType, Maybe String )
getBlockTypeAndLabel str =
    case ClassifyBlock.classify str of
        CBeginBlock label ->
            if List.member label verbatimBlockNames then
                ( PBVerbatim, Just label )

            else
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

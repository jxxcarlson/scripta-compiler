module Parser.PrimitiveBlock exposing
    ( PrimitiveBlock, empty, parse
    , argsAndProperties, elaborate, eq, length, listLength, parse_, print, toPrimitiveBlock
    )

{-| The main function is

    parse : Language -> (String -> Bool) -> List String -> List PrimitiveBlock

@docs PrimitiveBlock, empty, parse

-}

-- import MicroLaTeX.Expression.TransformLaTeX

import Dict exposing (Dict)
import List.Extra
import Parser.Line as Line exposing (Line, PrimitiveBlockType(..), isEmpty, isNonEmptyBlank)
import Parser.PrimitiveLaTeXBlock
import Scripta.Language exposing (Language(..))


{-| -}
type alias PrimitiveBlock =
    { indent : Int
    , lineNumber : Int
    , position : Int
    , content : List String
    , numberOfLines : Int
    , name : Maybe String
    , args : List String
    , properties : Dict String String
    , sourceText : String
    , blockType : PrimitiveBlockType
    , error : Maybe { error : String }
    }


length : PrimitiveBlock -> Int
length block =
    List.length block.content


listLength1 : List PrimitiveBlock -> Int
listLength1 blocks =
    (List.map length blocks |> List.sum) + List.length blocks - 1


listLength : List PrimitiveBlock -> Int
listLength blocks =
    case List.Extra.unconsLast blocks of
        Nothing ->
            0

        Just ( lastBlock, _ ) ->
            lastBlock.lineNumber + length lastBlock - 1


eq : PrimitiveBlock -> PrimitiveBlock -> Bool
eq b1 b2 =
    if b1.sourceText /= b2.sourceText then
        False

    else if b1.name /= b2.name then
        False

    else if b1.args /= b2.args then
        False

    else
        True


empty : PrimitiveBlock
empty =
    { indent = 0
    , lineNumber = 0
    , position = 0
    , content = [ "???" ]
    , numberOfLines = 1
    , name = Nothing
    , args = []
    , properties = Dict.empty
    , sourceText = "???"
    , blockType = PBParagraph
    , error = Nothing
    }


type alias State =
    { blocks : List PrimitiveBlock
    , currentBlock : Maybe PrimitiveBlock
    , lang : Language
    , lines : List String
    , inBlock : Bool
    , indent : Int
    , lineNumber : Int
    , position : Int
    , inVerbatim : Bool
    , isVerbatimLine : String -> Bool
    , count : Int
    , label : String
    }


{-| Parse a list of strings into a list of primitive blocks given a markup
language and a function for determining when a string is the first line
of a verbatim block
-}
parse : Language -> (String -> Bool) -> List String -> List PrimitiveBlock
parse lang isVerbatimLine lines =
    case lang of
        L0Lang ->
            lines |> parse_ lang isVerbatimLine

        MicroLaTeXLang ->
            -- lines |> MicroLaTeX.Expression.TransformLaTeX.toL0 |> parse_ lang isVerbatimLine
            lines |> Parser.PrimitiveLaTeXBlock.parse |> List.map toPrimitiveBlock

        PlainTextLang ->
            parsePlainText lines

        XMarkdownLang ->
            -- lines |> MicroLaTeX.Expression.TransformLaTeX.toL0 |> parse_ isVerbatimLine
            lines |> parse_ lang isVerbatimLine


toPrimitiveBlock : Parser.PrimitiveLaTeXBlock.PrimitiveLaTeXBlock -> PrimitiveBlock
toPrimitiveBlock block =
    { indent = block.level -- block.indent
    , lineNumber = block.lineNumber
    , position = block.position
    , content = block.content
    , numberOfLines = block.numberOfLines
    , name = block.name
    , args = block.args
    , properties = block.properties
    , sourceText = block.sourceText
    , blockType = block.blockType
    , error = block.error
    }


parsePlainText : List String -> List PrimitiveBlock
parsePlainText lines =
    let
        firstLines =
            List.take 2 lines

        rest =
            List.drop 2 lines

        --  |> List.Extra.dropWhile (\line -> line == "")
        title =
            if String.contains "| title" (List.head firstLines |> Maybe.withDefault "") then
                List.Extra.getAt 1 firstLines |> Maybe.withDefault "((no title))" |> String.trim

            else
                "((no title))"

        titleBLock =
            { empty
                | name = Just "title"
                , args = []
                , content = [ "| title", title, "" ]
                , sourceText = String.join "\n" lines
                , blockType = PBOrdinary
            }
    in
    titleBLock :: parsePlainText_ rest


parsePlainText_ : List String -> List PrimitiveBlock
parsePlainText_ lines =
    [ { indent = 0
      , lineNumber = 0
      , position = 0
      , content = lines
      , numberOfLines = List.length lines
      , name = Just "verbatim"
      , args = []
      , properties = Dict.empty
      , sourceText = String.join "\n" lines
      , blockType = PBVerbatim
      , error = Nothing
      }
    ]


parse_ : Language -> (String -> Bool) -> List String -> List PrimitiveBlock
parse_ lang isVerbatimLine lines =
    loop (init lang isVerbatimLine lines) nextStep
        |> List.map (\block -> finalize block)



-- TODO: think about the below


finalize : PrimitiveBlock -> PrimitiveBlock
finalize block =
    let
        content =
            List.reverse block.content

        sourceText =
            -- TODO: maybe this should be set at the Primitive block level
            String.join "\n" content
    in
    { block | content = content, numberOfLines = block.numberOfLines, sourceText = sourceText }


{-|

    Recall: classify position lineNumber, where position
    is the position of the first charabcter in the source
    and lineNumber is the index of the current line in the source

-}
init : Language -> (String -> Bool) -> List String -> State
init lang isVerbatimLine lines =
    { blocks = []
    , currentBlock = Nothing
    , lang = lang
    , lines = lines
    , indent = 0
    , lineNumber = 0
    , inBlock = False
    , position = 0
    , inVerbatim = False
    , isVerbatimLine = isVerbatimLine
    , count = 0
    , label = "0, START"
    }


blockFromLine : Language -> Line -> PrimitiveBlock
blockFromLine lang ({ indent, lineNumber, position, prefix, content } as line) =
    { indent = indent
    , lineNumber = lineNumber
    , position = position
    , content = [ prefix ++ content ]
    , numberOfLines = 1
    , name = Nothing
    , args = []
    , properties = Dict.empty -- TODO complete this
    , sourceText = ""
    , blockType = Line.getBlockType lang line.content
    , error = Nothing
    }
        |> elaborate lang line


nextStep : State -> Step State (List PrimitiveBlock)
nextStep state =
    case List.head state.lines of
        Nothing ->
            -- finish up: no more lines to process
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

                reportAction state_ currentLine_ =
                    case ( state_.inBlock, isEmpty currentLine_, isNonEmptyBlank currentLine_ ) of
                        ( False, True, _ ) ->
                            String.fromInt state_.lineNumber ++ ": advance" ++ " ++ :: " ++ currentLine_.content

                        ( False, False, True ) ->
                            String.fromInt state_.lineNumber ++ ": advance2 (PASS)" ++ " ++ :: " ++ currentLine_.content

                        ( False, False, False ) ->
                            String.fromInt state_.lineNumber ++ ": createBlock" ++ " ++ :: " ++ currentLine_.content

                        ( True, False, _ ) ->
                            String.fromInt state_.lineNumber ++ ": addCurrentLine2" ++ " ++ :: " ++ currentLine_.content

                        ( True, True, _ ) ->
                            String.fromInt state_.lineNumber ++ ": commitBlock" ++ " ++ :: " ++ currentLine_.content

                --_ =
                --    Debug.log (reportAction state currentLine) 1
            in
            case ( state.inBlock, isEmpty currentLine, isNonEmptyBlank currentLine ) of
                -- (in block, current line is empty, current line is blank but not empty)
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
                            { block_ | content = block_.content |> dropLast } |> adjustBlock

                        PBVerbatim ->
                            if List.head block_.content == Just "```" then
                                { block_ | content = List.filter (\l -> l /= "```") block_.content }

                            else
                                { block_ | content = dropLast block_.content }

                ( currentBlock, newBlocks ) =
                    if block.content == [ "" ] then
                        ( Nothing, state.blocks )

                    else
                        ( Just (blockFromLine state.lang currentLine), block :: state.blocks )
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


adjustBlock : PrimitiveBlock -> PrimitiveBlock
adjustBlock block =
    if block.name == Just "section" && block.args == [] then
        { block | args = [ "1" ] }

    else if block.name == Just "subsection" && block.args == [] then
        { block | args = [ "2" ] }

    else if block.name == Just "subsubsection" && block.args == [] then
        { block | args = [ "3" ] }

    else
        block


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
            Just (blockFromLine state.lang currentLine)
    in
    { state
        | lines = List.drop 1 state.lines
        , lineNumber = state.lineNumber + 1
        , position = state.position + String.length currentLine.content
        , count = state.count + 1
        , indent = currentLine.indent
        , inBlock = True
        , currentBlock = newBlock
        , blocks = blocks
    }


argsAndProperties : List String -> ( List String, Dict String String )
argsAndProperties words =
    let
        args =
            cleanArgs words

        namedArgs =
            List.drop (List.length args) words

        properties =
            namedArgs |> prepareList |> prepareKVData
    in
    ( words, properties )


elaborate : Language -> Line -> PrimitiveBlock -> PrimitiveBlock
elaborate lang line pb =
    if pb.content == [ "" ] then
        pb

    else
        let
            ( name, args_ ) =
                -- TODO: note this change: it needs to be verified
                Line.getNameAndArgs lang line

            ( args, properties ) =
                argsAndProperties args_

            content =
                if pb.blockType == PBVerbatim then
                    List.map String.trimLeft pb.content

                else
                    pb.content
        in
        { pb | content = content, name = name, args = args, properties = properties }


{-| return all the elements in the list 'strs' up to the first element contaiing ':'
This functio is used to return the positional arguments but not the named ones.
-}
cleanArgs : List String -> List String
cleanArgs strs =
    case List.Extra.findIndex (\t -> String.contains ":" t) strs of
        Nothing ->
            strs

        Just k ->
            List.take k strs


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


dropLast : List a -> List a
dropLast list =
    List.take (List.length list - 1) list


addCurrentLine_ : Line -> PrimitiveBlock -> PrimitiveBlock
addCurrentLine_ ({ prefix, content } as line) block =
    if block.blockType == PBVerbatim then
        if block.name == Just "math" then
            { block | content = line.content :: block.content, sourceText = block.sourceText ++ "\n" ++ prefix ++ content }

        else
            { block | content = (line.prefix ++ line.content) :: block.content, sourceText = block.sourceText ++ "\n" ++ prefix ++ content }

    else
        { block | content = line.content :: block.content, sourceText = block.sourceText ++ "\n" ++ prefix ++ content }


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


{-| Used for debugging with CLI.LOPB
-}
print : PrimitiveBlock -> String
print block =
    [ "BLOCK:"
    , "Type: " ++ Line.showBlockType block.blockType
    , "Name: " ++ showName block.name
    , "Indent: " ++ String.fromInt block.indent
    , "Args: " ++ showArgs block.args
    , "Properties: " ++ showProperties block.properties
    , "Error: " ++ showError block.error
    , "Line number: " ++ String.fromInt block.lineNumber
    , "Content: " --                                                                                                                                                                                                                                                                                                                                   ++ Debug.toString block.content
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


showError : Maybe { error : String } -> String
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

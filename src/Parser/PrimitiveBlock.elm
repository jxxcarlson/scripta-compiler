module Parser.PrimitiveBlock exposing
    ( PrimitiveBlock, empty, parse
    , eq, parse_
    )

{-| The main function is

    parse : Language -> (String -> Bool) -> List String -> List PrimitiveBlock

@docs PrimitiveBlock, empty, parse

-}

import List.Extra
import MicroLaTeX.Parser.TransformLaTeX
import Parser.Line as Line exposing (Line, PrimitiveBlockType(..), isEmpty, isNonEmptyBlank)
import Scripta.Language exposing (Language(..))


eq : PrimitiveBlock -> PrimitiveBlock -> Bool
eq b1 b2 =
    b1.content == b2.content


{-| -}
type alias PrimitiveBlock =
    { indent : Int
    , lineNumber : Int
    , position : Int
    , content : List String
    , name : Maybe String
    , args : List String
    , named : Bool
    , sourceText : String
    , blockType : PrimitiveBlockType
    }


empty : PrimitiveBlock
empty =
    { indent = 0
    , lineNumber = 0
    , position = 0
    , content = [ "???" ]
    , name = Nothing
    , args = []
    , named = False
    , sourceText = "???"
    , blockType = PBParagraph
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
            lines |> MicroLaTeX.Parser.TransformLaTeX.toL0 |> parse_ lang isVerbatimLine

        PlainTextLang ->
            parsePlainText lines

        XMarkdownLang ->
            -- lines |> MicroLaTeX.Parser.TransformLaTeX.toL0 |> parse_ isVerbatimLine
            lines |> parse_ lang isVerbatimLine


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
                , named = True
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
      , name = Just "verbatim"
      , args = []
      , named = True
      , sourceText = String.join "\n" lines
      , blockType = PBVerbatim
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
            String.join "\n" content
    in
    { block | content = content, sourceText = sourceText }


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
    , name = Nothing
    , args = []
    , named = False
    , sourceText = ""
    , blockType = Line.getBlockType lang line.content
    }
        |> elaborate line


nextStep : State -> Step State (List PrimitiveBlock)
nextStep state =
    case List.head state.lines of
        Nothing ->
            case state.currentBlock of
                Nothing ->
                    Done (List.reverse state.blocks)

                Just block ->
                    let
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
                    Just (addCurrentLine currentLine block)
            }


commitBlock : State -> Line -> State
commitBlock state currentLine =
    case state.currentBlock of
        Nothing ->
            { state
                | lines = List.drop 1 state.lines
                , indent = currentLine.indent
            }

        Just block ->
            let
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


addCurrentLine : Line -> PrimitiveBlock -> PrimitiveBlock
addCurrentLine line block =
    let
        pb =
            addCurrentLine_ line block
    in
    elaborate line pb


elaborate : Line -> PrimitiveBlock -> PrimitiveBlock
elaborate line pb =
    if pb.named then
        pb

    else if pb.content == [ "" ] then
        pb

    else
        let
            ( name, args ) =
                -- TODO: note this change: it needs to be verified
                Line.getNameAndArgs L0Lang line

            content =
                if pb.blockType == PBVerbatim then
                    List.map String.trimLeft pb.content

                else
                    pb.content
        in
        { pb | content = content, name = name, args = args, named = True }


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

module Render.Block exposing (render, renderVerbatimBlock)

import Compiler.ASTTools as ASTTools
import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input
import Html.Attributes
import List.Extra
import Maybe.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Render.Color as Color
import Render.Data
import Render.Expression
import Render.Graphics
import Render.Helper
import Render.IFrame
import Render.List
import Render.Math
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Sync
import Render.Tabular
import Render.Utility
import String.Extra
import Utility



-- TOPLEVEL


topPaddingForIndentedElements =
    10


render : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
render count acc settings ((ExpressionBlock { name, indent, args, error, blockType, content, id }) as block) =
    case blockType of
        Paragraph ->
            renderParagraph count acc settings block

        OrdinaryBlock _ ->
            renderOrdinaryBlock count acc settings block |> showError error

        VerbatimBlock _ ->
            renderVerbatimBlock count acc settings block |> showError error


renderParagraph : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderParagraph count acc settings (ExpressionBlock { name, indent, args, blockType, content, lineNumber, numberOfLines, id }) =
    case content of
        Right exprs ->
            List.map (Render.Expression.render count acc settings) exprs
                |> clickableParagraph lineNumber numberOfLines (selectedColor id settings)
                |> indentParagraph indent

        Left _ ->
            Element.none


indentParagraph : number -> Element msg -> Element msg
indentParagraph indent x =
    if indent > 0 then
        Element.el [ Element.paddingEach { top = topPaddingForIndentedElements, bottom = 0, left = 0, right = 0 } ] x

    else
        x


selectedColor id settings =
    if id == settings.selectedId then
        Background.color (Element.rgb 0.9 0.9 1.0)

    else
        Background.color settings.backgroundColor


clickableParagraph : Int -> Int -> Element.Attribute MarkupMsg -> List (Element MarkupMsg) -> Element MarkupMsg
clickableParagraph lineNumber numberOfLines color elements =
    let
        id =
            String.fromInt lineNumber
    in
    Element.paragraph
        [ color
        , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
        , htmlId id
        ]
        elements


renderOrdinaryBlock : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderOrdinaryBlock count acc settings ((ExpressionBlock { name, indent, error, args, blockType, content, lineNumber }) as block) =
    case content of
        Left _ ->
            Element.none

        Right _ ->
            case name of
                Nothing ->
                    noSuchOrdinaryBlock count acc settings block

                Just functionName ->
                    case Dict.get functionName blockDict of
                        Nothing ->
                            env count acc settings block
                                |> indentOrdinaryBlock indent (String.fromInt lineNumber) settings

                        Just f ->
                            f count acc settings block
                                |> indentOrdinaryBlock indent (String.fromInt lineNumber) settings


indentOrdinaryBlock : Int -> String -> Settings -> Element msg -> Element msg
indentOrdinaryBlock indent id settings x =
    if indent > 0 then
        Element.el [ selectedColor id settings, Element.paddingEach { top = topPaddingForIndentedElements, bottom = 0, left = 0, right = 0 } ] x

    else
        x



-- |> showError error


showError : Maybe { a | error : String } -> Element msg -> Element msg
showError error_ x =
    case error_ of
        Nothing ->
            x

        Just error ->
            Element.column []
                [ x
                , Element.el [ Font.color (Element.rgb 0.7 0 0) ] (Element.text error.error)
                ]


renderVerbatimBlock : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderVerbatimBlock count acc settings ((ExpressionBlock { name, error, indent, args, blockType, content, id }) as block) =
    case content of
        Right _ ->
            Element.none

        Left str ->
            case name of
                Nothing ->
                    noSuchVerbatimBlock "name" str

                Just functionName ->
                    case Dict.get functionName verbatimDict of
                        Nothing ->
                            noSuchVerbatimBlock functionName str

                        Just f ->
                            Element.el [ selectedColor id settings ] (f count acc settings block)



-- DICT OF BLOCKS


blockDict : Dict String (Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg)
blockDict =
    Dict.fromList
        [ ( "indent", indented )
        , ( "center", centered )
        , ( "box", box )
        , ( "quotation", quotation )
        , ( "set-key", \_ _ _ _ -> Element.none )
        , ( "comment", comment )
        , ( "q", question ) -- xx
        , ( "a", answer ) -- xx
        , ( "document", document )
        , ( "collection", collection )
        , ( "bibitem", bibitem )
        , ( "section", section ) -- xx
        , ( "subheading", subheading ) -- xx
        , ( "runninghead_", \_ _ _ _ -> Element.none ) -- DEPRECATED
        , ( "banner", \_ _ _ _ -> Element.none )
        , ( "title", \_ _ _ _ -> Element.none )
        , ( "subtitle", \_ _ _ _ -> Element.none )
        , ( "author", \_ _ _ _ -> Element.none )
        , ( "date", \_ _ _ _ -> Element.none )
        , ( "contents", \_ _ _ _ -> Element.none )
        , ( "tags", \_ _ _ _ -> Element.none )
        , ( "type", \_ _ _ _ -> Element.none )
        , ( "env", env_ )
        , ( "item", Render.List.item )
        , ( "desc", Render.List.desc )
        , ( "numbered", Render.List.numbered )
        , ( "index", index )
        , ( "endnotes", endnotes )
        , ( "setcounter", \_ _ _ _ -> Element.none )
        ]



-- IMPORTANT NOTE: all of the verbatim block names listed below
-- must be present in Parser.PrimitiveLaTeXBlock.verbatimNames


verbatimDict : Dict String (Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg)
verbatimDict =
    Dict.fromList
        [ ( "math", Render.Math.displayedMath )
        , ( "equation", Render.Math.equation )
        , ( "aligned", Render.Math.aligned )
        , ( "code", renderCode )
        , ( "verse", renderVerse )
        , ( "verbatim", renderVerbatim )
        , ( "tabular", Render.Tabular.render )
        , ( "hide", renderNothing )
        , ( "texComment", renderNothing )
        , ( "docinfo", renderNothing )
        , ( "mathmacros", renderNothing )
        , ( "textmacros", renderNothing )
        , ( "datatable", Render.Data.table )
        , ( "chart", Render.Data.chart )
        , ( "svg", Render.Graphics.svg )
        , ( "quiver", Render.Graphics.quiver )
        , ( "image", Render.Graphics.image2 )
        , ( "tikz", Render.Graphics.tikz )
        , ( "load-files", renderNothing )
        , ( "include", renderNothing )
        , ( "iframe", Render.IFrame.render )
        ]



-- ERRORS.


noSuchVerbatimBlock : String -> String -> Element MarkupMsg
noSuchVerbatimBlock functionName content =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "No such block: " ++ functionName ]
        , Element.column [ Element.spacing 4 ] (List.map (\t -> Element.el [] (Element.text t)) (String.lines content))
        ]


noSuchOrdinaryBlock : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
noSuchOrdinaryBlock count acc settings ((ExpressionBlock { args }) as block) =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "No such block:" ++ (args |> String.join " ") ]
        , Element.paragraph [] (List.map (Render.Expression.render count acc settings) (Parser.Block.getExprs block))
        ]


renderNothing : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderNothing _ _ _ _ =
    Element.none



-- DEFAULTS


renderWithDefaultWithSize : Int -> String -> Int -> Accumulator -> Settings -> List Expr -> List (Element MarkupMsg)
renderWithDefaultWithSize size default count acc settings exprs =
    if List.isEmpty exprs then
        [ Element.el [ Font.color settings.redColor, Font.size size ] (Element.text default) ]

    else
        List.map (Render.Expression.render count acc settings) exprs


renderWithDefault2 : String -> Int -> Accumulator -> Settings -> List Expr -> List (Element MarkupMsg)
renderWithDefault2 _ count acc settings exprs =
    List.map (Render.Expression.render count acc settings) exprs



-- HEADINGS


subheading : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
subheading count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    Element.link
        (sectionBlockAttributes block settings [ topPadding 10 ])
        { url = Render.Utility.internalLink (settings.titlePrefix ++ "title")
        , label = Element.paragraph [] (Render.Helper.renderWithDefault "| subheading" count acc settings (Parser.Block.getExprs block))
        }


section : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
section count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args, properties }) as block) =
    -- level 1 is reserved for titles
    let
        headingLevel =
            case args of
                n :: [] ->
                    if n == "-" then
                        2

                    else
                        String.toFloat n |> Maybe.withDefault 2 |> (\x -> x + 1)

                n :: "-" :: [] ->
                    String.toFloat n |> Maybe.withDefault 2 |> (\x -> x + 1)

                _ ->
                    3

        fontSize =
            settings.maxHeadingFontSize / sqrt headingLevel |> round

        sectionNumber =
            case args of
                n :: [] ->
                    if n == "-" then
                        Element.none

                    else
                        Element.el [ Font.size fontSize ] (Element.text (blockLabel properties ++ ". "))

                _ :: "-" :: [] ->
                    Element.none

                _ ->
                    Element.none

        exprs =
            Parser.Block.getExprs block
    in
    Element.link
        (sectionBlockAttributes block settings [ topPadding 20, Font.size fontSize ])
        { url = Render.Utility.internalLink (settings.titlePrefix ++ "title"), label = Element.paragraph [] (sectionNumber :: renderWithDefaultWithSize 18 "??" count acc settings exprs) }


sectionBlockAttributes : ExpressionBlock -> Settings -> List (Element.Attr () MarkupMsg) -> List (Element.Attr () MarkupMsg)
sectionBlockAttributes ((ExpressionBlock { lineNumber, numberOfLines, args, properties }) as block) settings attrs =
    [ Render.Utility.makeId (Parser.Block.getExprs block)
    , Render.Utility.idAttribute lineNumber
    ]
        ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings
        ++ attrs


topPadding : Int -> Element.Attribute msg
topPadding k =
    Element.paddingEach { top = k, bottom = 0, left = 0, right = 0 }



-- TODO: Below here
-- SCRIPTA
-- Some blocks are signals to Scripta.  There is nothing to render


{-|

    A block of the form "| collection" informs Scripta that the body
    of the document is a collection of links to other documents and
    that it should be interpreted as a kind of table of contents

    A collection document might look like this:

    | title
    Quantum Mechanics Notes

    [tags jxxcarlson:quantum-mechanics-notes, collection, system:startup, folder:krakow]

    | collection

    | document jxxcarlson:qmnotes-trajectories-uncertainty
    Trajectories and Uncertainty

    | document jxxcarlson:wave-packets-dispersion
    Wave Packets and the Dispersion Relation

    | document jxxcarlson:wave-packets-schroedinger
    Wave Packets and Schrödinger's Equation

-}
collection : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
collection _ _ _ _ =
    Element.none


{-|

    Use a document block to include another document in a collection, e.g,

        | document jxxcarlson:wave-packets-schroedinger
        Wave Packets and Schrödinger's Equation

-}
document : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
document _ _ settings ((ExpressionBlock { id, args, properties }) as block) =
    let
        docId =
            case args |> List.head of
                Just idx ->
                    idx

                Nothing ->
                    case properties |> Dict.toList |> List.head |> Maybe.map (\( a, b ) -> a ++ ":" ++ b) of
                        Just ident ->
                            ident

                        Nothing ->
                            "(noId)"

        level =
            List.Extra.getAt 1 args |> Maybe.withDefault "1" |> String.toInt |> Maybe.withDefault 1

        title =
            List.map ASTTools.getText (Parser.Block.getExprs block) |> Maybe.Extra.values |> String.join " " |> Utility.truncateString 35

        sectionNumber =
            case Dict.get "label" properties of
                Just "-" ->
                    "- "

                Just s ->
                    s ++ ". "

                Nothing ->
                    "- "
    in
    Element.row
        [ Element.alignTop
        , Render.Utility.elementAttribute "id" settings.selectedId
        , Render.Utility.vspace 0 settings.topMarginForChildren
        , Element.moveRight (15 * (level - 1) |> toFloat)
        , fontColor settings.selectedId settings.selectedSlug docId
        ]
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Element.width (Element.px 30)
            ]
            (Element.text sectionNumber)
        , ilink title settings.selectedId settings.selectedSlug docId
        ]



-- DEPRECATE?? WE ARE USING THE ELEMENT VERSION


ilink : String -> String -> Maybe String -> String -> Element MarkupMsg
ilink docTitle selectedId selecteSlug docId =
    Element.Input.button []
        { onPress = Just (GetPublicDocument Render.Msg.MHStandard docId)

        -- { onPress = Just (GetDocumentById docId)
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.size 14
                , fontColor selectedId selecteSlug docId
                ]
                (Element.text docTitle)
        }



-- QUESTIONS AND ANSWERS (FOR TEACHING)


question : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
question count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args, properties }) as block) =
    let
        id =
            String.fromInt lineNumber

        title =
            String.join " " args

        label =
            " " ++ getLabel properties

        qId =
            Dict.get id acc.qAndADict |> Maybe.withDefault id
    in
    Element.column [ Element.spacing 12 ]
        [ Element.el [ Font.bold, Font.color Color.blue, Events.onClick (HighlightId qId) ] (Element.text (title ++ " " ++ label))
        , Element.paragraph ([ Font.italic, Events.onClick (HighlightId qId), Render.Utility.idAttribute lineNumber ] ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings)
            (Render.Helper.renderWithDefault "..." count acc settings (Parser.Block.getExprs block))
        ]


answer : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
answer count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    let
        id =
            String.fromInt lineNumber

        _ =
            ( id, args )

        title =
            String.join " " (List.drop 1 args)

        clicker =
            if settings.selectedId == id then
                Events.onClick (ProposeSolution Render.Msg.Unsolved)

            else
                Events.onClick (ProposeSolution (Render.Msg.Solved id))
    in
    Element.column [ Element.spacing 12, Element.paddingEach { top = 0, bottom = 24, left = 0, right = 0 } ]
        [ Element.el [ Font.bold, Font.color Color.blue, clicker ] (Element.text title)
        , if settings.selectedId == id then
            Element.el [ Events.onClick (ProposeSolution Render.Msg.Unsolved) ]
                (Element.paragraph ([ Font.italic, Render.Utility.idAttribute lineNumber, Element.paddingXY 8 8 ] ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings)
                    (Render.Helper.renderWithDefault "..." count acc settings (Parser.Block.getExprs block))
                )

          else
            Element.none
        ]



-- LATEXY STUFF


{-|

    Used to render generic LaTeX environments

-}
env_ : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
env_ count acc settings ((ExpressionBlock { name, lineNumber, numberOfLines, indent, args, blockType, content, properties }) as block) =
    case List.head args of
        Nothing ->
            Element.paragraph
                [ Render.Utility.idAttribute lineNumber
                , Font.color settings.redColor
                , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
                ]
                [ Element.text "| env (missing name!)" ]

        Just _ ->
            env count acc settings block


{-|

    Used to render generic LaTeX environments

-}
env : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
env count acc settings (ExpressionBlock { name, indent, args, blockType, content, lineNumber, numberOfLines, properties }) =
    case content of
        Left _ ->
            Element.none

        Right exprs ->
            Element.column ([ Element.spacing 8, Render.Utility.idAttribute lineNumber ] ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings)
                [ Element.el
                    [ Font.bold
                    , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
                    ]
                    (Element.text (blockHeading name args properties))
                , Element.paragraph
                    [ Font.italic
                    , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
                    ]
                    (renderWithDefault2 ("??" ++ (name |> Maybe.withDefault "(name)")) count acc settings exprs)
                ]


{-|

    Used in function env (ender generic LaTeX environments).
    This function numbers blocks for which there is a "label" property

-}
blockHeading : Maybe String -> List String -> Dict String String -> String
blockHeading name args properties =
    if List.member name [ Just "banner_", Just "banner" ] then
        ""

    else
        (name |> Maybe.withDefault "(name)" |> String.Extra.toTitleCase)
            ++ " "
            ++ (Dict.get "label" properties |> Maybe.withDefault "")
            ++ " "
            ++ String.join " " args


{-|

    Used in function env (ender generic LaTeX environments)

-}
blockLabel : Dict String String -> String
blockLabel properties =
    Dict.get "label" properties |> Maybe.withDefault "??"



-- VARIOUS BLOCKS


{-| indented block
-}
indented : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
indented count acc settings ((ExpressionBlock { lineNumber, numberOfLines }) as block) =
    Element.paragraph
        ([ Render.Utility.leftPadding settings.leftIndentation
         , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
         , Render.Utility.idAttribute lineNumber
         ]
            ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings
        )
        (Render.Helper.renderWithDefault "indent" count acc settings (Parser.Block.getExprs block))


centered : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
centered count acc settings ((ExpressionBlock { lineNumber, numberOfLines }) as block) =
    Element.paragraph
        ([ Element.width (Element.px (settings.width - 60))
         , Element.centerX
         , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
         , Render.Utility.idAttribute lineNumber
         ]
            ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings
        )
        (Render.Helper.renderWithDefault "indent" count acc settings (Parser.Block.getExprs block))


box : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
box count acc settings ((ExpressionBlock { lineNumber, numberOfLines, name, args, properties }) as block) =
    Element.column [ Element.paddingXY 48 0 ]
        [ Element.column
            ([ Background.color Color.lightBlue
             , Element.padding 20
             , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
             , Render.Utility.idAttribute lineNumber
             , Element.spacing 18
             ]
                ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings
            )
            [ Element.el [ Font.bold ] (Element.text (blockHeading name args properties))
            , Element.paragraph
                []
                (Render.Helper.renderWithDefault "box" count acc settings (Parser.Block.getExprs block))
            ]
        ]


{-| -}
comment count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    let
        author_ =
            String.join " " args

        author =
            if author_ == "" then
                ""

            else
                author_ ++ ":"
    in
    Element.column [ Element.spacing 6 ]
        [ Element.el [ Font.bold, Font.color Color.blue ] (Element.text author)
        , Element.paragraph ([ Font.italic, Font.color Color.blue, Render.Sync.rightLeftSyncHelper lineNumber numberOfLines, Render.Utility.idAttribute lineNumber ] ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings)
            (Render.Helper.renderWithDefault "| comment" count acc settings (Parser.Block.getExprs block))
        ]



--- (lineNumber + numberOfLines)
-- TODO: attrs!


quotation count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args, properties }) as block) =
    Element.column [ Element.spacing 12 ]
        [ Element.paragraph
            (blockAttributes settings block [ Render.Utility.leftPadding settings.leftIndentation, Font.italic ])
            (Render.Helper.renderWithDefault "!!! (quotation)" count acc settings (Parser.Block.getExprs block))
        ]


blockAttributes settings (ExpressionBlock { lineNumber, numberOfLines, args, properties }) attrs =
    [ Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
    , Render.Utility.idAttribute lineNumber
    ]
        ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings
        ++ attrs


bibitem : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
bibitem count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    let
        label =
            List.Extra.getAt 0 args |> Maybe.withDefault "(12)" |> (\s -> "[" ++ s ++ "]")
    in
    Element.row ([ Element.alignTop, Render.Utility.idAttribute lineNumber, Render.Utility.vspace 0 settings.topMarginForChildren ] ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings)
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Font.bold
            , Element.width (Element.px 34)
            ]
            (Element.text label)
        , Element.paragraph ([ Element.paddingEach { left = 25, right = 0, top = 0, bottom = 0 }, Render.Sync.rightLeftSyncHelper lineNumber numberOfLines ] ++ Render.Sync.highlightIfIdIsSelected lineNumber numberOfLines settings)
            (Render.Helper.renderWithDefault "bibitem" count acc settings (Parser.Block.getExprs block))
        ]



-- <iframe src="https://www.desmos.com/calculator/ycaswggsgb?embed" width="500" height="500" style="border: 1px solid #ccc" frameborder=0></iframe>
-- VERBATIM


renderCode : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderCode count acc settings ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    Element.column
        [ Font.color settings.codeColor
        , Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]

        --, Element.spacing 8
        , Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
        , Render.Utility.idAttribute lineNumber
        ]
        (case List.head args of
            Just arg ->
                List.map (renderVerbatimLine arg) (String.lines (String.trim (Render.Utility.getVerbatimContent block)))

            Nothing ->
                List.map (renderVerbatimLine "plain") (String.lines (String.trim (Render.Utility.getVerbatimContent block)))
        )


renderVerse : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderVerse _ _ _ ((ExpressionBlock { lineNumber, numberOfLines }) as block) =
    Element.column
        (verbatimBlockAttributes lineNumber numberOfLines [])
        (List.map (renderVerbatimLine "plain") (String.lines (String.trim (Render.Utility.getVerbatimContent block))))


renderVerse1 : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderVerse1 _ _ _ ((ExpressionBlock { lineNumber, numberOfLines }) as block) =
    Element.column
        (verbatimBlockAttributes lineNumber numberOfLines [ Element.htmlAttribute (Html.Attributes.attribute "whitespace" "pre") ])
        (List.map (renderVerbatimLine "plain") (String.lines (String.trim (Render.Utility.getVerbatimContent block)))
            |> padFirst 9
        )


padFirst : Int -> List (Element MarkupMsg) -> List (Element MarkupMsg)
padFirst leftPadding elements =
    case elements of
        [] ->
            []

        first :: rest ->
            Element.el [ Render.Utility.leftPadding leftPadding ] first :: rest


verbatimBlockAttributes lineNumber numberOfLines attrs =
    [ Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
    , Render.Utility.idAttribute lineNumber
    ]
        ++ attrs


renderVerbatimLine : String -> String -> Element msg
renderVerbatimLine lang str =
    if String.trim str == "" then
        Element.el [ Element.height (Element.px 11) ] (Element.text "")

    else if lang == "plain" then
        Element.el [ Element.height (Element.px 22) ] (Element.text str)

    else
        Element.paragraph [ Element.height (Element.px 22) ] (renderedColoredLine lang str)


renderedColoredLine lang str =
    str
        |> String.words
        |> List.map (renderedColoredWord lang)


renderedColoredWord lang word =
    case lang of
        "elm" ->
            case Dict.get word elmDict of
                Just color ->
                    Element.el [ color ] (Element.text (word ++ " "))

                Nothing ->
                    Element.el [] (Element.text (word ++ " "))

        _ ->
            Element.el [] (Element.text (word ++ " "))


orange =
    Font.color (Element.rgb255 227 81 18)


green =
    Font.color (Element.rgb255 11 158 26)


cyan =
    Font.color (Element.rgb255 11 143 158)


elmDict =
    Dict.fromList
        [ ( "type", orange )
        , ( "LB", green )
        , ( "RB", green )
        , ( "S", green )
        , ( "String", green )
        , ( "Meta", cyan )
        ]


renderVerbatim : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
renderVerbatim _ _ _ ((ExpressionBlock { lineNumber, numberOfLines, args }) as block) =
    let
        _ =
            List.head args
    in
    Element.column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Element.spacing 8
        , Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        , Render.Sync.rightLeftSyncHelper lineNumber numberOfLines
        , Render.Utility.idAttribute lineNumber
        ]
        (case List.head args of
            Just lang ->
                List.map (renderVerbatimLine lang) (String.lines (String.trim (Render.Utility.getVerbatimContent block)))

            _ ->
                List.map (renderVerbatimLine "none") (String.lines (String.trim (Render.Utility.getVerbatimContent block)))
        )


getLabel : Dict String String -> String
getLabel dict =
    Dict.get "label" dict |> Maybe.withDefault ""


labeledArgs : List String -> String
labeledArgs args =
    List.map (\s -> String.replace "label:" "" s) args |> String.join " "



-- INDEX


index : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
index _ acc _ (ExpressionBlock { lineNumber, numberOfLines, args }) =
    let
        groupItemList : List GroupItem
        groupItemList =
            acc.terms
                |> Dict.toList
                |> List.map (\( name, item_ ) -> ( String.trim name, item_ ))
                |> List.sortBy (\( name, _ ) -> name)
                |> List.Extra.groupWhile (\a b -> String.left 1 (Tuple.first a) == String.left 1 (Tuple.first b))
                |> List.map (\thing -> group thing)
                |> List.concat

        groupItemListList : List (List GroupItem)
        groupItemListList =
            groupItemList
                |> List.Extra.greedyGroupsOf 30
                |> List.map normalize
    in
    Element.row [ Element.spacing 18 ] (List.map renderGroup groupItemListList)


indexItem : GroupItem -> Element MarkupMsg
indexItem groupItem =
    case groupItem of
        GBlankLine ->
            Element.el [ Element.height (Element.px 8) ] (Element.text "")

        GItem item_ ->
            indexItem_ item_


indexItem_ : Item -> Element MarkupMsg
indexItem_ ( name, loc ) =
    Element.link [ Font.color (Element.rgb 0 0 0.8), Events.onClick (SelectId loc.id) ]
        { url = Render.Utility.internalLink loc.id, label = Element.el [] (Element.text (String.toLower name)) }



-- ENDNOTES


endnotes : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
endnotes _ acc _ (ExpressionBlock { lineNumber, numberOfLines, args }) =
    let
        endnoteList =
            acc.footnotes
                |> Dict.toList
                |> List.map
                    (\( content, meta ) ->
                        { label = Dict.get meta.id acc.footnoteNumbers |> Maybe.withDefault 0
                        , content = content
                        , id = meta.id ++ "_"
                        }
                    )
                |> List.sortBy .label
    in
    Element.column [ Element.spacing 12 ]
        (Element.el [ Font.bold ] (Element.text "Endnotes")
            :: List.map renderFootnote endnoteList
        )


renderFootnote : { label : Int, content : String, id : String } -> Element MarkupMsg
renderFootnote { label, content, id } =
    Element.paragraph [ Element.spacing 4 ]
        [ Element.el [ htmlId id, Element.width (Element.px 24) ] (Element.text (String.fromInt label ++ "."))
        , Element.text content
        ]



-- GROUP ???


renderGroup : List GroupItem -> Element MarkupMsg
renderGroup groupItems =
    Element.column [ Element.alignTop, Element.spacing 6, Element.width (Element.px 150) ] (List.map indexItem groupItems)


normalize gp =
    case List.head gp of
        Just GBlankLine ->
            List.drop 1 gp

        Just (GItem _) ->
            gp

        Nothing ->
            gp


group : ( Item, List Item ) -> List GroupItem
group ( item_, list ) =
    GBlankLine :: GItem item_ :: List.map GItem list


type GroupItem
    = GBlankLine
    | GItem Item


type alias Item =
    ( String, { begin : Int, end : Int, id : String } )


fontColor selectedId selectedSlug docId =
    if selectedId == docId then
        Font.color (Element.rgb 0.8 0 0)

    else if selectedSlug == Just docId then
        Font.color (Element.rgb 0.8 0 0)

    else
        Font.color (Element.rgb 0 0 0.9)


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)

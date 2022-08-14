module Render.Block exposing (render)

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
import Render.Elm
import Render.Graphics
import Render.Math
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Utility
import String.Extra



-- TOPLEVEL


render : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
render count acc settings (ExpressionBlock { name, args, blockType, content, id }) =
    case blockType of
        Paragraph ->
            case content of
                Right exprs ->
                    let
                        color =
                            if id == settings.selectedId then
                                Background.color (Element.rgb 0.9 0.9 1.0)

                            else
                                Background.color settings.backgroundColor
                    in
                    List.map (Render.Elm.render count acc settings) exprs
                        |> (\x -> Element.paragraph [ color, Events.onClick (SendId id), htmlId id ] x)

                Left _ ->
                    Element.none

        OrdinaryBlock _ ->
            case content of
                Left _ ->
                    Element.none

                Right exprs ->
                    case name of
                        Nothing ->
                            noSuchOrdinaryBlock count acc settings "name" exprs

                        Just functionName ->
                            case Dict.get functionName blockDict of
                                Nothing ->
                                    env (String.Extra.toTitleCase functionName) count acc settings args id exprs

                                Just f ->
                                    f count acc settings args id exprs

        VerbatimBlock _ ->
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
                                    f count acc settings args id str



-- DICT OF BLOCKS


blockDict : Dict String (Int -> Accumulator -> Settings -> List String -> String -> List Expr -> Element MarkupMsg)
blockDict =
    Dict.fromList
        [ ( "indent", indented )
        , ( "quotation", quotation )
        , ( "comment", comment )
        , ("key", \_ _ _ _ _ _ -> Element.none)
        , ( "q", question ) -- xx
        , ( "a", answer ) -- xx
        , ( "document", document )
        , ( "collection", collection )
        , ( "bibitem", bibitem )
        , ( "section", section ) -- xx
        , ( "subheading", subheading ) -- xx
        , ( "runninghead", \_ _ _ _ _ _ -> Element.none )
        , ( "runninghead_", runninghead ) -- ??
        , ( "banner", \_ _ _ _ _ _ -> Element.none )
        , ( "banner_", banner ) -- ??
        , ( "title", \_ _ _ _ _ _ -> Element.none )
        , ( "subtitle", \_ _ _ _ _ _ -> Element.none )
        , ( "author", \_ _ _ _ _ _ -> Element.none )
        , ( "date", \_ _ _ _ _ _ -> Element.none )
        , ( "contents", \_ _ _ _ _ _ -> Element.none )
        , ( "tags", \_ _ _ _ _ _ -> Element.none )
        , ( "type", \_ _ _ _ _ _ -> Element.none )
        , ( "env", env_ )
        , ( "item", item )
        , ( "desc", desc )
        , ( "numbered", numbered )
        , ( "index", index )
        , ( "endnotes", endnotes )
        , ( "setcounter", \_ _ _ _ _ _ -> Element.none )
        ]


verbatimDict : Dict String (Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg)
verbatimDict =
    Dict.fromList
        [ ( "math", Render.Math.displayedMath )
        , ( "equation", Render.Math.equation )
        , ( "aligned", Render.Math.aligned )
        , ( "code", renderCode )
        , ( "verse", renderVerse )
        , ( "verbatim", renderVerbatim )
        , ( "hide", renderNothing )
        , ( "docinfo", renderNothing )
        , ( "mathmacros", renderNothing )
        , ( "textmacros", renderNothing )
        , ( "datatable", Render.Data.table )
        , ( "chart", Render.Data.chart )
        , ( "svg", Render.Graphics.svg )
        , ( "quiver", Render.Graphics.quiver )
        , ( "tikz", Render.Graphics.tikz )
        , ( "load-files", \_ _ _ _ _ _ -> Element.none )
        , ( "include", \_ _ _ _ _ _ -> Element.none )
        ]



-- ERRORS


noSuchVerbatimBlock : String -> String -> Element MarkupMsg
noSuchVerbatimBlock functionName content =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "|| " ++ functionName ++ " ??(8)" ]
        , Element.column [ Element.spacing 4 ] (List.map (\t -> Element.el [] (Element.text t)) (String.lines content))
        ]


noSuchOrdinaryBlock : Int -> Accumulator -> Settings -> String -> List Expr -> Element MarkupMsg
noSuchOrdinaryBlock count acc settings functionName exprs =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "| " ++ functionName ++ " ??(9) " ]
        , Element.paragraph [] (List.map (Render.Elm.render count acc settings) exprs)
        ]


renderNothing : Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg
renderNothing _ _ _ _ _ _ =
    Element.none



-- DEFAULTS


renderWithDefault : String -> Int -> Accumulator -> Settings -> List Expr -> List (Element MarkupMsg)
renderWithDefault default count acc settings exprs =
    if List.isEmpty exprs then
        [ Element.el [ Font.color Render.Settings.redColor, Font.size 14 ] (Element.text default) ]

    else
        List.map (Render.Elm.render count acc settings) exprs


renderWithDefault2 : String -> Int -> Accumulator -> Settings -> List Expr -> List (Element MarkupMsg)
renderWithDefault2 _ count acc settings exprs =
    List.map (Render.Elm.render count acc settings) exprs



-- HEADINGS


subheading count acc settings _ id exprs =
    Element.link
        ([ Font.size 16
         , Font.bold
         , Render.Utility.makeId exprs
         , Render.Utility.elementAttribute "id" id
         , Events.onClick (SendId "title")
         , Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 }
         ]
            ++ highlightAttrs id settings
        )
        { url = Render.Utility.internalLink (settings.titlePrefix ++ "title"), label = Element.paragraph [] (renderWithDefault "| subheading" count acc settings exprs) }


section count acc settings args id exprs =
    -- level 1 is reserved for titles
    let
        headingLevel =
            case List.head args of
                Nothing ->
                    3

                Just level ->
                    String.toFloat level |> Maybe.withDefault 2 |> (\x -> x + 1)

        sectionNumber =
            case List.Extra.getAt 1 args of
                Just "-" ->
                    Element.none

                Just s ->
                    Element.el [ Font.size fontSize ] (Element.text (s ++ ". "))

                Nothing ->
                    Element.none

        fontSize =
            Render.Settings.maxHeadingFontSize / sqrt headingLevel |> round
    in
    Element.link
        ([ Font.size fontSize
         , Render.Utility.makeId exprs
         , Render.Utility.elementAttribute "id" id
         , Events.onClick (SendId "title")
         , Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
         ]
            ++ highlightAttrs id settings
        )
        { url = Render.Utility.internalLink (settings.titlePrefix ++ "title"), label = Element.paragraph [] (sectionNumber :: renderWithDefault "| section" count acc settings exprs) }



-- SCRIPTA


runninghead count acc settings _ id exprs =
    Element.paragraph ([ Events.onClick (SendId id), Render.Utility.elementAttribute "id" id ] ++ highlightAttrs id settings)
        (renderWithDefault "| runninghead" count acc settings exprs)


banner count acc settings _ id exprs =
    Element.paragraph ([ Events.onClick (SendId id), Render.Utility.elementAttribute "id" id ] ++ highlightAttrs id settings)
        (renderWithDefault "| banner" count acc settings exprs)


collection _ _ _ _ _ _ =
    Element.none


document _ _ settings args selectedId exprs =
    let
        docId =
            List.Extra.getAt 0 args |> Maybe.withDefault "--"

        level =
            List.Extra.getAt 1 args |> Maybe.withDefault "1" |> String.toInt |> Maybe.withDefault 1

        title =
            List.map ASTTools.getText exprs |> Maybe.Extra.values |> String.join " " |> truncateString 35

        sectionNumber =
            case List.Extra.getAt 2 args of
                Just "-" ->
                    "- "

                Just s ->
                    s ++ ". "

                Nothing ->
                    "- "
    in
    Element.row
        [ Element.alignTop
        , Render.Utility.elementAttribute "id" selectedId
        , vspace 0 Render.Settings.topMarginForChildren
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


question count acc settings args id exprs =
    let
        title =
            String.join " " (List.drop 1 args)

        label =
            List.take 1 args |> String.join ""
    in
    Element.column [ Element.spacing 12 ]
        [ Element.el [ Font.bold ] (Element.text (title ++ " " ++ label))
        , Element.paragraph ([ Font.italic, Events.onClick (SendId id), Render.Utility.elementAttribute "id" id ] ++ highlightAttrs id settings)
            (renderWithDefault "..." count acc settings exprs)
        ]


answer count acc settings args id exprs =
    let
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
                (Element.paragraph ([ Font.italic, Render.Utility.elementAttribute "id" id, Element.paddingXY 8 8 ] ++ highlightAttrs id settings)
                    (renderWithDefault "..." count acc settings exprs)
                )

          else
            Element.none
        ]



-- LATEXY STUFF


env_ : Int -> Accumulator -> Settings -> List String -> String -> List Expr -> Element MarkupMsg
env_ count acc settings args id exprs =
    case List.head args of
        Nothing ->
            Element.paragraph [ Render.Utility.elementAttribute "id" id, Font.color Render.Settings.redColor, Events.onClick (SendId id) ] [ Element.text "| env (missing name!)" ]

        Just name ->
            env name count acc settings (List.drop 1 args) id exprs


env : String -> Int -> Accumulator -> Settings -> List String -> String -> List Expr -> Element MarkupMsg
env name count acc settings args id exprs =
    let
        label =
            args
                |> List.filter (\s -> String.contains "index::" s)
                |> String.join ""
                |> String.replace "index::" ""

        headingString =
            String.join " " (List.filter (\s -> not (String.contains "::" s)) args)

        envHeading =
            name ++ " " ++ label ++ headingString
    in
    Element.column ([ Element.spacing 8, Render.Utility.elementAttribute "id" id ] ++ highlightAttrs id settings)
        [ Element.el [ Font.bold, Events.onClick (SendId id) ] (Element.text envHeading)
        , Element.paragraph [ Font.italic, Events.onClick (SendId id) ]
            (renderWithDefault2 ("| " ++ name) count acc settings exprs)
        ]


indented count acc settings _ id exprs =
    Element.paragraph ([ Render.Settings.leftIndentation, Events.onClick (SendId id), Render.Utility.elementAttribute "id" id ] ++ highlightAttrs id settings)
        (renderWithDefault "| indent" count acc settings exprs)


comment count acc settings args id exprs =
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
        , Element.paragraph ([ Font.italic, Events.onClick (SendId id), Render.Utility.elementAttribute "id" id ] ++ highlightAttrs id settings)
            (renderWithDefault "| indent" count acc settings exprs)
        ]


quotation count acc settings args id exprs =
    let
        attribution_ =
            String.join " " args

        attribution =
            if attribution_ == "" then
                ""

            else
                "—" ++ attribution_
    in
    Element.column [ Element.spacing 12 ]
        [ Element.paragraph ([ Font.italic, Render.Settings.leftIndentation, Events.onClick (SendId id), Render.Utility.elementAttribute "id" id ] ++ highlightAttrs id settings)
            (renderWithDefault "| indent" count acc settings exprs)
        , Element.el [ Render.Settings.wideLeftIndentation, Font.italic ] (Element.text attribution)
        ]


bibitem : Int -> Accumulator -> Settings -> List String -> String -> List Expr -> Element MarkupMsg
bibitem count acc settings args id exprs =
    let
        label =
            List.Extra.getAt 0 args |> Maybe.withDefault "(12)" |> (\s -> "[" ++ s ++ "]")
    in
    Element.row ([ Element.alignTop, Render.Utility.elementAttribute "id" id, vspace 0 Render.Settings.topMarginForChildren ] ++ highlightAttrs id settings)
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Font.bold
            , Element.width (Element.px 34)
            ]
            (Element.text label)
        , Element.paragraph ([ Element.paddingEach { left = 25, right = 0, top = 0, bottom = 0 }, Events.onClick (SendId id) ] ++ highlightAttrs id settings)
            (renderWithDefault "bibitem" count acc settings exprs)
        ]


highlightAttrs id settings =
    if id == settings.selectedId then
        [ Events.onClick (SendId id), Background.color (Element.rgb 0.8 0.8 1.0) ]

    else
        [ Events.onClick (SendId id) ]



-- VERBATIM


renderCode : Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg
renderCode _ _ _ args id str =
    let
        _ =
            List.head args
    in
    Element.column
        [ Font.color Render.Settings.codeColor
        , Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]

        --, Element.spacing 8
        , Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        , Events.onClick (SendId id)
        , Render.Utility.elementAttribute "id" id
        ]
        (case List.head args of
            Just arg ->
                List.map (renderVerbatimLine arg) (String.lines (String.trim str))

            Nothing ->
                List.map (renderVerbatimLine "plain") (String.lines (String.trim str))
        )


renderVerse : Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg
renderVerse _ _ _ _ id str =
    Element.column
        [ Events.onClick (SendId id)
        , Render.Utility.elementAttribute "id" id
        ]
        (List.map (renderVerbatimLine "plain") (String.lines (String.trim str)))


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


renderVerbatim : Int -> Accumulator -> Settings -> List String -> String -> String -> Element MarkupMsg
renderVerbatim _ _ _ args id str =
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
        , Events.onClick (SendId id)
        , Render.Utility.elementAttribute "id" id
        ]
        (case List.head args of
            Just lang ->
                List.map (renderVerbatimLine lang) (String.lines (String.trim str))

            _ ->
                List.map (renderVerbatimLine "none") (String.lines (String.trim str))
        )



-- LISTS


item count acc settings _ id exprs =
    let
        level =
            Dict.get id acc.numberedItemDict |> Maybe.map .level |> Maybe.withDefault 0

        label =
            case modBy 3 level of
                0 ->
                    String.fromChar '●'

                1 ->
                    String.fromChar '○'

                _ ->
                    "◊"
    in
    Element.row [ Element.moveRight (indentationScale * level |> toFloat), Element.alignTop, Render.Utility.elementAttribute "id" id, vspace 0 Render.Settings.topMarginForChildren ]
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Element.moveRight 6
            , Element.width (Element.px 24)
            , Render.Settings.leftIndentation
            ]
            (Element.text label)
        , Element.paragraph [ Render.Settings.leftIndentation, Events.onClick (SendId id) ]
            (renderWithDefault "| item" count acc settings exprs)
        ]


numbered count acc settings _ id exprs =
    let
        alphabet =
            [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]

        romanNumerals =
            [ "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx", "xi", "xxii", "xxiii", "xxiv", "xxv", "vi" ]

        alpha k =
            List.Extra.getAt (modBy 26 (k - 1)) alphabet |> Maybe.withDefault "a"

        roman k =
            List.Extra.getAt (modBy 26 (k - 1)) romanNumerals |> Maybe.withDefault "i"

        val =
            Dict.get id acc.numberedItemDict

        index_ =
            val |> Maybe.map .index |> Maybe.withDefault 1

        level =
            val |> Maybe.map .level |> Maybe.withDefault 0

        label =
            case modBy 3 level of
                1 ->
                    alpha index_

                2 ->
                    roman index_

                _ ->
                    String.fromInt index_
    in
    Element.row [ Element.moveRight (indentationScale * level |> toFloat), Element.alignTop, Render.Utility.elementAttribute "id" id, vspace 0 Render.Settings.topMarginForChildren ]
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Element.width (Element.px 24)
            , Render.Settings.leftRightIndentation
            ]
            (Element.text (label ++ ". "))
        , Element.paragraph [ Render.Settings.leftIndentation, Events.onClick (SendId id) ]
            (renderWithDefault "| numbered" count acc settings exprs)
        ]


desc count acc settings args id exprs =
    let
        label : String
        label =
            String.join " " args
    in
    Element.row ([ Element.alignTop, Render.Utility.elementAttribute "id" id, vspace 0 Render.Settings.topMarginForChildren ] ++ highlightAttrs id settings)
        [ Element.el [ Font.bold, Element.alignTop, Element.width (Element.px 100) ] (Element.text label)
        , Element.paragraph [ Render.Settings.leftIndentation, Events.onClick (SendId id) ]
            (renderWithDefault "| desc" count acc settings exprs)
        ]


indentationScale =
    15



-- INDEX


index _ acc _ _ _ _ =
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
--type alias Item =
--    ( String, { begin : Int, end : Int, id : String } )


endnotes _ acc _ _ _ _ =
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



-- ???


vspace =
    Render.Utility.vspace



-- HELPERS


truncateString : Int -> String -> String
truncateString k str =
    let
        str2 =
            truncateString_ k str
    in
    if str == str2 then
        str

    else
        str2 ++ " ..."


truncateString_ : Int -> String -> String
truncateString_ k str =
    if String.length str < k then
        str

    else
        let
            words =
                String.words str

            n =
                List.length words
        in
        words
            |> List.take (n - 1)
            |> String.join " "
            |> truncateString_ k


fontColor selectedId selectedSlug docId =
    if selectedId == docId then
        Font.color (Element.rgb 0.8 0 0)

    else if selectedSlug == Just docId then
        Font.color (Element.rgb 0.8 0 0)

    else
        Font.color (Element.rgb 0 0 0.9)


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)

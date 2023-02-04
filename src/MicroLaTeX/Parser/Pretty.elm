module MicroLaTeX.Parser.Pretty exposing (idempotencyTest, print, printForest, roundTripTest, weakRoundTripTest)

import Compiler.DifferentialParser
import Dict
import MicroLaTeX.Parser.Transform
import MicroLaTeX.Parser.TransformLaTeX
import Parser.Block
import Parser.Expr
import Parser.Forest
import Parser.Meta
import Parser.Settings
import Scripta.Language
import Tree exposing (Tree)


roundTripTest : String -> Bool
roundTripTest input =
    String.trim input == print input


weakRoundTripTest : String -> Bool
weakRoundTripTest input =
    compress (String.trim input) == compress (print input)


idempotencyTest : String -> Bool
idempotencyTest input =
    String.trim input == (input |> print |> print)


compress str =
    String.replace "\n" "" str


pseudoBlockNames =
    MicroLaTeX.Parser.TransformLaTeX.pseudoBlockNames ++ MicroLaTeX.Parser.Transform.pseudoBlockNamesWithContent


print : String -> String
print input =
    input
        |> Compiler.DifferentialParser.init Dict.empty Scripta.Language.MicroLaTeXLang
        |> .parsed
        |> Compiler.DifferentialParser.forestFromBlocks
        |> printForest
        |> String.trim


printForest : Parser.Forest.Forest Parser.Block.ExpressionBlock -> String
printForest forest =
    List.map printTree forest |> String.join "" |> String.trim


printTree : Tree Parser.Block.ExpressionBlock -> String
printTree tree =
    let
        blockName =
            Parser.Block.getName (Tree.label tree)
                |> Maybe.withDefault "---"

        root : Parser.Block.ExpressionBlock
        root =
            Tree.label tree
    in
    if List.member blockName Parser.Settings.numberedBlockNames then
        unravel tree

    else
        unravel tree


unravel : Tree Parser.Block.ExpressionBlock -> String
unravel tree =
    let
        root =
            Tree.label tree

        name =
            Parser.Block.getName root |> Maybe.withDefault "(anon)"

        rootContent =
            Parser.Block.getContent root

        children =
            Tree.children tree
    in
    if List.isEmpty children then
        printBlock root

    else
        case Parser.Block.getBlockType root of
            Parser.Block.OrdinaryBlock _ ->
                (printOrdinaryBlock name rootContent :: List.map unravel children)
                    ++ [ endTag name ]
                    |> String.join "\n"
                    |> appendDoubleNewline

            _ ->
                (printBlock root :: List.map unravel children) |> String.join "\n" |> appendDoubleNewline


printOrdinaryBlock : String -> List Parser.Expr.Expr -> String
printOrdinaryBlock name exprs =
    [ beginTag name, printExprs exprs ] |> String.join "\n"


printLineNumber : Parser.Block.ExpressionBlock -> String
printLineNumber (Parser.Block.ExpressionBlock data) =
    String.fromInt data.lineNumber ++ ":\n"


printBlock : Parser.Block.ExpressionBlock -> String
printBlock ((Parser.Block.ExpressionBlock data) as block) =
    (case Parser.Block.getBlockType block of
        Parser.Block.Paragraph ->
            printLineNumber block ++ (block |> Parser.Block.getContent |> printExprs) ++ "\n"

        Parser.Block.OrdinaryBlock args ->
            let
                name =
                    block |> Parser.Block.getName |> Maybe.withDefault "(anon)"

                content =
                    block |> Parser.Block.getContent
            in
            (if List.member name MicroLaTeX.Parser.TransformLaTeX.pseudoBlockNames then
                printLineNumber block ++ "\\" ++ name ++ " " ++ printExprs content

             else if List.member name MicroLaTeX.Parser.Transform.pseudoBlockNamesWithContent then
                printLineNumber block ++ macro name content

             else
                case data.error of
                    Nothing ->
                        printLineNumber block ++ ([ beginTag name, printExprs content, endTag name ] |> String.join "\n")

                    Just e ->
                        printLineNumber block ++ ([ beginTag name, printExprs content, endTag name, "ERROR: " ++ e.error ] |> String.join "\n")
            )
                ++ "\n"

        Parser.Block.VerbatimBlock args ->
            let
                content =
                    block |> Parser.Block.getVerbatimContent

                name =
                    block |> Parser.Block.getName |> Maybe.withDefault "(anon)"
            in
            printLineNumber block ++ ([ beginTag name, content, endTag name ] |> String.join "\n")
    )
        ++ "\n"


beginTag : String -> String
beginTag str =
    "\\begin{" ++ str ++ "}"


endTag : String -> String
endTag str =
    "\\end{" ++ str ++ "}"


macro : String -> List Parser.Expr.Expr -> String
macro name args =
    ("\\" :: name :: List.map (printExpr >> arg) args) |> String.join ""


macroTrimmed : String -> List Parser.Expr.Expr -> String
macroTrimmed name args =
    ("\\" :: name :: List.map (printExpr >> arg >> String.trim) args) |> String.join ""


arg : String -> String
arg str =
    "{" ++ str ++ "}"


printExprs : List Parser.Expr.Expr -> String
printExprs exprs =
    List.map printExpr exprs |> String.join ""


printExpr1 : Parser.Expr.Expr -> String
printExpr1 expr =
    case expr of
        Parser.Expr.Text str _ ->
            str

        Parser.Expr.Fun name exprs _ ->
            macro name exprs

        Parser.Expr.Verbatim name body _ ->
            case name of
                "math" ->
                    [ "$", body, "$" ] |> String.join ""

                "code" ->
                    [ "`", body, "`" ] |> String.join ""

                _ ->
                    [ name, body ] |> String.join " "


printMeta : Parser.Meta.Meta -> String
printMeta meta =
    "[" ++ String.fromInt meta.begin ++ "-" ++ String.fromInt meta.end ++ "] "


printExpr : Parser.Expr.Expr -> String
printExpr expr =
    case expr of
        Parser.Expr.Text str meta ->
            str ++ printMeta meta

        Parser.Expr.Fun name exprs meta ->
            macro name exprs ++ printMeta meta

        Parser.Expr.Verbatim name body meta ->
            case name of
                "math" ->
                    ([ "$", body, "$" ] |> String.join "") ++ printMeta meta

                "code" ->
                    ([ "`", body, "`" ] |> String.join "") ++ printMeta meta

                _ ->
                    ([ name, body ] |> String.join " ") ++ printMeta meta


appendNewline : String -> String
appendNewline str =
    str ++ "\n"


appendDoubleNewline : String -> String
appendDoubleNewline str =
    str ++ "\n\n"

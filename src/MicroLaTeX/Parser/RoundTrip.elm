module MicroLaTeX.Parser.RoundTrip exposing (test)

import Compiler.DifferentialParser
import Dict
import MicroLaTeX.Parser.Transform
import MicroLaTeX.Parser.TransformLaTeX
import Parser.Block
import Parser.Expr
import Parser.Forest
import Parser.Settings
import Scripta.Language
import Tree exposing (Tree)


pseudoBlockNames =
    MicroLaTeX.Parser.TransformLaTeX.pseudoBlockNames ++ MicroLaTeX.Parser.Transform.pseudoBlockNamesWithContent


test : String -> String
test input =
    input
        |> Compiler.DifferentialParser.init Dict.empty Scripta.Language.MicroLaTeXLang
        |> .parsed
        |> Compiler.DifferentialParser.forestFromBlocks
        |> print


print : Parser.Forest.Forest Parser.Block.ExpressionBlock -> String
print forest =
    List.map printTree forest |> String.join "\n\n"


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
                (printOrdinaryBlock name rootContent :: List.map unravel children) ++ [ endTag name ] |> String.join "\n\n"

            _ ->
                (printBlock root :: List.map unravel children) |> String.join "\n\n"


printOrdinaryBlock : String -> List Parser.Expr.Expr -> String
printOrdinaryBlock name exprs =
    [ beginTag name, printExprs exprs ] |> String.join "\n"


printBlock : Parser.Block.ExpressionBlock -> String
printBlock block =
    case Parser.Block.getBlockType block of
        Parser.Block.Paragraph ->
            block |> Parser.Block.getContent |> printExprs

        Parser.Block.OrdinaryBlock args ->
            let
                name =
                    block |> Parser.Block.getName |> Maybe.withDefault "(anon)"

                content =
                    block |> Parser.Block.getContent
            in
            if List.member name MicroLaTeX.Parser.TransformLaTeX.pseudoBlockNames then
                "\\" ++ name ++ " " ++ printExprs content

            else if List.member name MicroLaTeX.Parser.Transform.pseudoBlockNamesWithContent then
                macro name content

            else
                [ beginTag name, printExprs content, endTag name ] |> String.join "\n"

        Parser.Block.VerbatimBlock args ->
            let
                content =
                    block |> Parser.Block.getVerbatimContent

                name =
                    block |> Parser.Block.getName |> Maybe.withDefault "(anon)"
            in
            [ beginTag name, content, endTag name ] |> String.join "\n"


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


printExpr : Parser.Expr.Expr -> String
printExpr expr =
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

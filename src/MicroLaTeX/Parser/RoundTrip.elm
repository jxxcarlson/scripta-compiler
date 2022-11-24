module MicroLaTeX.Parser.RoundTrip exposing (test)

import Compiler.DifferentialParser
import Dict
import Parser.Block
import Parser.Expr
import Parser.Forest
import Parser.Settings
import Scripta.Language
import Tree exposing (Tree)


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

        children =
            Tree.children tree
    in
    if List.isEmpty children then
        printBlock root

    else
        (printBlock root :: List.map unravel children) |> String.join "\n\n"


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
                    block |> Parser.Block.getContent |> printExprs
            in
            [ beginTag name, content, endTag name ] |> String.join "\n"

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


printExprs : List Parser.Expr.Expr -> String
printExprs exprs =
    List.map printExpr exprs |> String.join " "


printExpr : Parser.Expr.Expr -> String
printExpr expr =
    case expr of
        Parser.Expr.Text str _ ->
            str

        Parser.Expr.Fun name exprs _ ->
            name :: List.map printExpr exprs |> String.join " "

        Parser.Expr.Verbatim name body _ ->
            [ name, body ] |> String.join " "

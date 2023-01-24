module XMPB exposing (..)

import Compiler.DifferentialParser
import Dict
import Markup
import MicroLaTeX.Parser.Pretty
import Parser.Block
import Parser.PrimitiveBlock exposing (PrimitiveBlock, parse_)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc
import Scripta.Language


program : Process -> IO ()
program process =
    case process.argv of
        [ _, filename ] ->
            IO.do
                (File.contentsOf filename
                    |> IO.exitOnError identity
                )
            <|
                \content ->
                    let
                        parsed : List Parser.Block.ExpressionBlock
                        parsed =
                            content |> Compiler.DifferentialParser.init Dict.empty Scripta.Language.XMarkdownLang |> .parsed

                        blockString_ =
                            parsed
                                |> Compiler.DifferentialParser.forestFromBlocks
                                |> MicroLaTeX.Parser.Pretty.printForest

                        blockString =
                            [ "", "================", blockString_, "================", "" ] |> String.join "\n"
                    in
                    IO.do (Proc.print blockString) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

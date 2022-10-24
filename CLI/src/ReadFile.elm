module ReadFile exposing (program)

import Dict exposing (Dict)
import Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parse)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


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
                        parsed =
                            content |> String.lines |> parse

                        blockString =
                            "\n----------------\nBLOCKS\n----------------\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.blocks |> String.join "\n\n")

                        stackString =
                            "\n\n----------------\nSTACK\n----------------\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.stack |> String.join "\n\n")
                                ++ "\n----------------\n"
                    in
                    IO.do (Proc.print (blockString ++ stackString)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

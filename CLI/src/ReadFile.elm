module ReadFile exposing (program)

import Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parse_)
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
                            content |> String.lines |> parse_

                        blockString =
                            "\n----------------\nBLOCKS\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.blocks |> String.join "\n\n")

                        holdingStackString =
                            "\n\n----------------\nHOLDING STACK\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.holdingStack |> String.join "\n\n")

                        stackString =
                            "\n\n----------------\nSTACK\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.stack |> String.join "\n\n")
                                ++ "\n----------------\n"
                    in
                    IO.do (Proc.print (blockString ++ holdingStackString ++ stackString)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

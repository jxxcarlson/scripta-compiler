module LXPBErr exposing (program)

import Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parseLoop)
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
                            content |> String.lines |> parseLoop

                        blockString =
                            List.map Parser.PrimitiveLaTeXBlock.printErr parsed.blocks |> String.join "\n\n"
                    in
                    IO.do (Proc.print blockString) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

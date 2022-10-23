module ReadFile exposing (program)

import Dict exposing (Dict)
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
                    -- IO.do (Proc.print content) <|
                    let
                        parsed : List PrimitiveLaTeXBlock
                        parsed =
                            content |> String.lines |> parse_ (\_ -> False)

                        out =
                            Debug.toString parsed
                    in
                    IO.do (Proc.print out) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

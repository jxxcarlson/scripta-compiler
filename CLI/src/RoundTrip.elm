module RoundTrip exposing (..)

import MicroLaTeX.Parser.RoundTrip
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


analyze : String -> String
analyze input =
    let
        compress str =
            String.replace "\n" "" str

        output =
            MicroLaTeX.Parser.RoundTrip.test input

        match =
            if input == output then
                "Exact match:    YES"

            else
                "Exact match:    NO"

        inexactMatch =
            if compress input == compress output then
                "Inexact match:  YES"

            else
                "Inexact match:  NO"
    in
    [ match, inexactMatch, "------", output ] |> String.join "\n"


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
                    IO.do (Proc.print (analyze content)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

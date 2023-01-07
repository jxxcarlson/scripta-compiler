module RoundTrip exposing (..)

import MicroLaTeX.Parser.Pretty
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


analyze : String -> String
analyze input =
    let
        compress str =
            String.replace "\n" "" str

        output =
            MicroLaTeX.Parser.Pretty.print input

        output2 =
            MicroLaTeX.Parser.Pretty.print output

        match =
            if MicroLaTeX.Parser.Pretty.roundTripTest input then
                "Exact match:    YES"

            else
                "Exact match:    NO"

        inexactMatch =
            if MicroLaTeX.Parser.Pretty.weakRoundTripTest input then
                "Inexact match:  YES"

            else
                "Inexact match:  NO"

        idempotencyMatch =
            if MicroLaTeX.Parser.Pretty.idempotencyTest input then
                "Idempotency match:  YES"

            else
                "Idempotency match:  NO"
    in
    [ input, "------", match, inexactMatch, idempotencyMatch, "------", output, "------", output2 ] |> String.join "\n"


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

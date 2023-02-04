module LXEB exposing (..)

import MicroLaTeX.Parser.Pretty
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


analyze : String -> String -> String
analyze opt input =
    case opt of
        "rt" ->
            analyze1 input

        "p" ->
            analyze2 input

        _ ->
            "Unknown option"


analyze1 : String -> String
analyze1 input =
    let
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
    [ ""
    , "================"
    , "Round trip test"
    , "==============="
    , input
    , "------"
    , match
    , inexactMatch
    , idempotencyMatch
    , "------"
    , output
    , "------"
    , output2
    , ""
    ]
        |> String.join "\n"


analyze2 : String -> String
analyze2 input =
    let
        output =
            MicroLaTeX.Parser.Pretty.print input
    in
    [ ""
    , "=========================="
    , "Parse to Expression blocks"
    , "=========================="
    , input
    , "------"
    , output
    , ""
    ]
        |> String.join "\n"


program : Process -> IO ()
program process =
    case process.argv of
        [ _, filename, opt ] ->
            IO.do
                (File.contentsOf filename
                    |> IO.exitOnError identity
                )
            <|
                \content ->
                    IO.do (Proc.print (analyze opt content)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file [p|rt]\n"

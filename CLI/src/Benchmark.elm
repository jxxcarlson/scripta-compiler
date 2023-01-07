module Benchmark exposing (program)

import Dict
import Element exposing (Element)
import Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parseLoop)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc
import Render.Msg
import Scripta.API
import Scripta.Language



-- EXAMPLE:
-- elm-cli run src/Benchmark.elm init 100 bench/harmonic.tex


program : Process -> IO ()
program process =
    case process.argv of
        [ _, testName, repetitions, filename ] ->
            IO.do
                (File.contentsOf filename
                    |> IO.exitOnError identity
                )
            <|
                \content ->
                    IO.do (Proc.print (bench testName repetitions content)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr errText


errText =
    """
Usage: elm-cli run src/Benchmark.elm OPTION N PATH, where

       OPTION = [pb, expr, compiile]
       N = number of repetitions
       PATH = path to file e.g., bench/harmonic.tex
"""


bench : String -> String -> String -> String
bench testName repetitions_ content =
    let
        testFunction =
            case testName of
                "pb" ->
                    benchPrimitiveLaTeXBlock

                "init" ->
                    benchMicroLaTeXApiInit

                "compile" ->
                    benchMicroLaTeXCompiler

                _ ->
                    \a b -> ()
    in
    case String.toInt repetitions_ of
        Nothing ->
            "Invalid number"

        Just repetitions ->
            let
                _ =
                    testFunction repetitions content
            in
            "Done"


repeat : Int -> a -> (a -> b) -> ()
repeat n input transform =
    if n == 0 then
        ()

    else
        let
            _ =
                transform input
        in
        repeat (n - 1) input transform


bench1 repetitions content =
    let
        input =
            String.lines content
    in
    repeat repetitions input parseLoop


benchPrimitiveLaTeXBlock repetitions content =
    repeat repetitions (String.lines content) parseLoop


benchMicroLaTeXCompiler repetitions content =
    repeat repetitions content compile


benchMicroLaTeXApiInit repetitions content =
    repeat repetitions content (Scripta.API.init Dict.empty Scripta.Language.MicroLaTeXLang)


compile : String -> List (Element Render.Msg.MarkupMsg)
compile str =
    Scripta.API.compile displaySettings Scripta.Language.MicroLaTeXLang str


displaySettings =
    { windowWidth = 700
    , counter = 0
    , selectedId = "nada"
    , selectedSlug = Nothing
    , scale = 1.0
    }

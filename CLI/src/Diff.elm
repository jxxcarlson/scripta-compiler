module Diff exposing (program)

import Compiler.Differ
import Compiler.DifferentialParser
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
-- elm-cli run src/Benchmark.elm expr 100 bench/harmonic.tex


program : Process -> IO ()
program process =
    case process.argv of
        [ _, testName, repetitions, filename1, filename2 ] ->
            IO.do
                (IO.combine [ File.contentsOf filename1 |> IO.exitOnError identity, File.contentsOf filename2 |> IO.exitOnError identity ])
            <|
                \result ->
                    case result of
                        [ content1, content2 ] ->
                            IO.do (Proc.print (bench testName repetitions content1 content2)) <|
                                \_ ->
                                    IO.return ()

                        _ ->
                            IO.return ()

        _ ->
            Proc.logErr errText


errText =
    """
Usage: elm-cli run src/Benchmark.elm OPTION N PATH1 PATH2, where

       OPTION = [diff, diffp]
       N = number of repetitions
       PATH* = path to file e.g., bench/harmonic.tex
"""


bench : String -> String -> String -> String -> String
bench testName repetitions_ content1 content2 =
    let
        testFunction =
            case testName of
                "diff" ->
                    diff

                "diffp" ->
                    diffp

                _ ->
                    \a b c -> ()
    in
    case String.toInt repetitions_ of
        Nothing ->
            "Invalid number"

        Just repetitions ->
            let
                _ =
                    testFunction repetitions content1 content2
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


repeat2 : Int -> a -> a -> (a -> a -> b) -> ()
repeat2 n input1 input2 transform =
    if n == 0 then
        ()

    else
        let
            _ =
                transform input1 input2
        in
        repeat2 (n - 1) input1 input2 transform


diff : Int -> String -> String -> ()
diff repetitions content1 content2 =
    let
        input1 =
            String.lines content1

        input2 =
            String.lines content2

        pb1 =
            parseLoop input1 |> .blocks

        pb2 =
            parseLoop input2 |> .blocks
    in
    repeat2 repetitions pb1 pb2 Compiler.Differ.diff


diffp : Int -> String -> String -> ()
diffp repetitions content1 content2 =
    let
        editRecord1 =
            Compiler.DifferentialParser.init Dict.empty Scripta.Language.MicroLaTeXLang content1
    in
    repeat repetitions content2 (\c -> Compiler.DifferentialParser.update editRecord1 c)


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

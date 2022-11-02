module Benchmark2 exposing (..)

import Element exposing (Element)
import Parser.PrimitiveLaTeXBlock exposing (PrimitiveLaTeXBlock, parse_)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc
import Render.Msg
import Scripta.API
import Scripta.Language


program : Process -> IO ()
program process =
    case process.argv of
        [ _, repetitions, filename ] ->
            IO.do
                (File.contentsOf filename
                    |> IO.exitOnError identity
                )
            <|
                \content ->
                    IO.do (Proc.print (bench repetitions content)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"


bench : String -> String -> String
bench repetitions_ content =
    case String.toInt repetitions_ of
        Nothing ->
            "Invalid number"

        Just repetitions ->
            let
                _ =
                    bench1a repetitions content
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
    repeat repetitions input parse_


bench1a repetitions content =
    repeat repetitions (String.lines content) parse_


bench2 repetitions content =
    let
        input =
            String.lines content
    in
    repeat repetitions input parse_


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

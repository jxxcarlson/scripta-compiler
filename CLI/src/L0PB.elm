module L0PB exposing (..)

import Markup
import Parser.PrimitiveBlock exposing (PrimitiveBlock, parse_)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc
import Scripta.Language exposing (Language(..))


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
                            content |> String.lines |> parse_ L0Lang Markup.isVerbatimLine

                        blockString =
                            "\n----------------\nBLOCKS\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveBlock.print parsed |> String.join "\n\n")
                    in
                    IO.do (Proc.print blockString) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

module LXPB exposing (program)

import MicroLaTeX.Parser.Transform
import Parser.PrimitiveBlock
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
                            "\n----------------\nBLOCKS\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.blocks
                                        |> String.join "\n\n"
                                   )

                        blockString2 =
                            let
                                primitiveBlocks =
                                    parsed.blocks |> List.map (Parser.PrimitiveBlock.toPrimitiveBlock >> MicroLaTeX.Parser.Transform.transform)
                            in
                            "\n----------------\nBLOCKS (Transformed)\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveBlock.print primitiveBlocks
                                        |> String.join "\n\n"
                                   )

                        holdingStackString =
                            "\n\n----------------\nHOLDING STACK\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.holdingStack |> String.join "\n\n")

                        stackString =
                            "\n\n----------------\nSTACK\n----------------\n\n"
                                ++ (List.map Parser.PrimitiveLaTeXBlock.print parsed.stack |> String.join "\n\n")
                                ++ "\n----------------\n"
                    in
                    IO.do (Proc.print (blockString ++ blockString2 ++ holdingStackString ++ stackString)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"

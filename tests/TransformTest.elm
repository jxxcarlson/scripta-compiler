module TransformTest exposing (..)

import Expect exposing (equal)
import Markup
import MicroLaTeX.Parser.Transform exposing (transform)
import Parser.Line
import Scripta.Language exposing (..)
import Test exposing (Test, describe, test)


test_ label expr expected =
    test label <| \_ -> equal expr expected


toPrimitiveBlocks =
    Markup.toPrimitiveBlocks MicroLaTeXLang


suite : Test
suite =
    describe "MicroLaTeX.Expression.Transform"
        [ test_ "transform, args"
            (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |> List.map .args)
            [ [ "1" ] ]
        , test_ "transform, name"
            (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |> List.map .name)
            [ Just "section" ]
        , test_ "transform, content"
            (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |> List.map .content)
            [ [ "Intro" ] ]
        , test_ "transform, blockType"
            (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |> List.map .blockType)
            [ Parser.Line.PBOrdinary ]
        ]

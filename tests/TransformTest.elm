module TransformTest exposing (..)


import Expect exposing (equal)
import MicroLaTeX.Parser.Transform exposing (transform)
import Test exposing (Test, describe, test)
import Markup
import Scripta.Language exposing(..)

test_ label expr expected =
    test label <| \_ -> equal expr expected

toPrimitiveBlocks =
    Markup.toPrimitiveBlocks MicroLaTeXLang

suite : Test
suite =
    Test.only <| describe "MicroLaTeX.Parser.Transform"
        [ test_ "transform, args"
             (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |>  List.map .args)
             (toPrimitiveBlocks "\n\n| section 1\n Intro\n\n" |> List.map .args)
         , test_ "transform, name"
            (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |>  List.map .name)
            (toPrimitiveBlocks "\n\n| section 1\n Intro\n\n" |> List.map .name)
        , test_ "transform, content"
            (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |>  List.map .content)
            (toPrimitiveBlocks "\n\n| section 1\n Intro\n\n" |> List.map .content)
        , test_ "transform, blockType"
            (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |>  List.map .blockType)
            (toPrimitiveBlocks "\n\n| section 1\n Intro\n\n" |> List.map .blockType)

      ]


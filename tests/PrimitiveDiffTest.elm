module PrimitiveDiffTest exposing (..)

import Compiler.Differ
import Compiler.PrimitiveBlock
import Expect
import Parser.PrimitiveBlock
import Parser.PrimitiveLaTeXBlock
import Scripta.Language
import Test exposing (Test)



-- exposing (Test, describe, test)


diff : String -> String -> Compiler.Differ.DiffRecord Parser.PrimitiveBlock.PrimitiveBlock
diff =
    Compiler.PrimitiveBlock.diff Scripta.Language.MicroLaTeXLang


checkSuffix : String -> String -> Maybe Int
checkSuffix source1 source2 =
    Maybe.map .lineNumber (List.head (diff source1 source2).commonSuffix)


diffTest : String -> Maybe Int -> String -> String -> Test
diffTest label mExpectedLineNumber source1 source2 =
    Test.test label <| \_ -> Expect.equal mExpectedLineNumber (checkSuffix source1 source2)


noChangeTest : String -> String -> String -> Test
noChangeTest label source1 source2 =
    Test.test label <| \_ -> Expect.equal (Compiler.PrimitiveBlock.noChange Scripta.Language.MicroLaTeXLang source1 source2) True


lengthTest : String -> String -> List Int -> Test
lengthTest label source lineNumbers =
    let
        blocks =
            source |> String.lines |> Parser.PrimitiveLaTeXBlock.parse |> List.map Parser.PrimitiveBlock.toPrimitiveBlock
    in
    Test.test label <| \_ -> Expect.equal lineNumbers (List.map .lineNumber blocks)


diffSuite : Test
diffSuite =
    Test.describe "MicroLaTeX diffing for primitive blocks"
        [ diffTest "a1-a1" Nothing a1 a1
        , diffTest "a1-a2" (Just 8) a1 a2
        , diffTest "a1-a2x" (Just 11) a1 a2x
        , diffTest "a2-a1" (Just 7) a2 a1
        , diffTest "a1-a3" (Just 8) a1 a3
        , diffTest "a3-a1" (Just 7) a3 a1
        , diffTest "a1-a4" (Just 9) a1 a4
        , diffTest "a4-a1" (Just 7) a4 a1

        -- TODO: is the below the correct interpretation?
        , diffTest "a5-a1" Nothing a5 a1
        , noChangeTest "a5-a1, no change" a5 a1
        , noChangeTest "a1-a5, no change" a1 a5

        -- TODO: is the below the correct interpretation?
        , diffTest "a1-a5" Nothing a1 a5
        ]


lengthSuite : Test
lengthSuite =
    Test.describe "Computation of line numbers"
        [ lengthTest "a1" a1 [ 1, 4, 7 ]
        , lengthTest "a4" a4 [ 1, 4, 7, 9 ]
        , lengthTest "a5" a5 [ 1, 8, 11 ]
        ]


a1 =
    """
a1
a2

b1
b2

c1
c2
"""


a5 =
    """
a1
a2





b1
b2

c1
c2
"""


a2 =
    """
a1
a2

b1
b2
b3

c1
c2
"""


a2x =
    """
a1
a2

b1
b2
b3




c1
c2
"""


a3 =
    """
a1
a2

b1

b2

c1
c2
"""


a4 =
    """
a1
a2

b1


b2

c1
c2
"""

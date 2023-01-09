module PrijmitiveDiffTest exposing (..)

import Compiler.Differ
import Compiler.PrimitiveBlock
import Parser.PrimitiveBlock
import Scripta.Language
import Test



-- exposing (Test, describe, test)


diff : String -> String -> Compiler.Differ.DiffRecord Parser.PrimitiveBlock.PrimitiveBlock
diff =
    Compiler.PrimitiveBlock.diff Scripta.Language.MicroLaTeXLang



--diffTest : String -> String -> String -> Test
--diffTest label source1 source2  =
-- Test.test label <| \_ -> Test.equal (diffB a b) c

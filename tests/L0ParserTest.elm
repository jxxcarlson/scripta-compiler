module L0ParserTest exposing (happy, unhappy)

import Expect exposing (equal)
import L0.Test
import Test exposing (Test, describe, test)


roundTrip1 text =
    test (text ++ ":1") <| \_ -> equal (L0.Test.check1 text) (Ok text)


checkErrorHandling text output =
    test (text ++ ":error") <| \_ -> equal (L0.Test.checkErrorHandling text output) (Ok output)


happy : Test
happy =
     describe "L0 Parser, round trip-tests, happy path"
        [ roundTrip1 "abc"
        , roundTrip1 "[foo]"
        , roundTrip1 "[foo bar]"
        , roundTrip1 "yada [foo bar] baz"
        , roundTrip1 "[yada [foo bar]] baz"
        ]


unhappy : Test
unhappy =
    Test.skip <| describe "L0 Parser, round trip-tests, unhappy path"
        [ checkErrorHandling "[foo" "[errorHighlight[foo][errorHighlight]?]"
        ]

module Compiler.DifferEq exposing (diff)

import Compiler.Differ exposing (DiffRecord)


{-| Let u and v be two lists of strings. Write them as
u = axb, v = ayb, where a is the greatest common prefix
and b is the greatest common suffix. Return DiffRecord a b x y
-}
diff : (q -> q -> Bool) -> List q -> List q -> DiffRecord q
diff eq u v =
    let
        a =
            commonInitialSegment eq u v

        b_ =
            commonTerminalSegmentAux eq a u v

        la =
            List.length a

        lb =
            List.length b_

        x =
            u |> List.drop la |> dropLast lb

        y =
            v |> List.drop la |> dropLast lb

        b =
            if la == List.length u then
                []

            else
                b_

        _ =
            List.map List.length [ a, b, x, y ]
    in
    DiffRecord a b x y


commonInitialSegment : (q -> q -> Bool) -> List q -> List q -> List q
commonInitialSegment eq x y =
    if x == [] then
        []

    else if y == [] then
        []

    else
        case ( List.head x, List.head y ) of
            ( Just a, Just b ) ->
                if eq a b then
                    a :: commonInitialSegment eq (List.drop 1 x) (List.drop 1 y)

                else
                    []

            _ ->
                []


commonTerminalSegmentAux : (q -> q -> Bool) -> List q -> List q -> List q -> List q
commonTerminalSegmentAux eq cis x y =
    let
        n =
            List.length cis

        xx =
            List.drop n x |> List.reverse

        yy =
            List.drop n y |> List.reverse
    in
    commonInitialSegment eq xx yy |> List.reverse


dropLast : Int -> List a -> List a
dropLast k x =
    x |> List.reverse |> List.drop k |> List.reverse


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse

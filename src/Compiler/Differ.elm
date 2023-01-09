module Compiler.Differ exposing (DiffRecord, diff, differentialTransform)


type alias DiffRecord p =
    { commonPrefix : List p
    , commonSuffix : List p
    , middleSegmentInSource : List p
    , middleSegmentInTarget : List p
    }


{-| Update the renderedList by applying the transformer only to the
changed source elements.
-}
differentialTransform : (p -> q) -> DiffRecord p -> List q -> List q
differentialTransform transform diffRecord renderedList =
    let
        prefixLengh =
            List.length diffRecord.commonPrefix

        suffixLength =
            List.length diffRecord.commonSuffix

        renderedPrefix =
            List.take prefixLengh renderedList

        renderedSuffix =
            takeLast suffixLength renderedList
    in
    renderedPrefix ++ List.map transform diffRecord.middleSegmentInTarget ++ renderedSuffix


{-| Let u and v be two lists of strings. Write them as
u = axb, v = ayb, where a is the greatest common prefix
and b is the greatest common suffix. Return DiffRecord a b x y
-}
diff : List p -> List p -> DiffRecord p
diff u v =
    let
        a =
            commonInitialSegment u v

        b_ =
            commonTerminalSegmentAux a u v

        la =
            List.length a

        lb =
            List.length b_

        x =
            u |> List.drop la |> dropLast lb

        y =
            v |> List.drop la |> dropLast lb

        b =
            List.drop (List.length a + List.length y) v

        -- TODO: does this work?
        b1 =
            if la == List.length u then
                []

            else
                b_

        _ =
            List.map List.length [ a, b, x, y ]
    in
    DiffRecord a b x y


commonInitialSegment : List p -> List p -> List p
commonInitialSegment x y =
    if x == [] then
        []

    else if y == [] then
        []

    else
        let
            a =
                List.take 1 x

            b =
                List.take 1 y
        in
        if a == b then
            a ++ commonInitialSegment (List.drop 1 x) (List.drop 1 y)

        else
            []


commonTerminalSegmentAux : List p -> List p -> List p -> List p
commonTerminalSegmentAux cis x y =
    let
        n =
            List.length cis

        xx =
            List.drop n x |> List.reverse

        yy =
            List.drop n y |> List.reverse
    in
    commonInitialSegment xx yy |> List.reverse


dropLast : Int -> List p -> List p
dropLast k x =
    x |> List.reverse |> List.drop k |> List.reverse


takeLast : Int -> List p -> List p
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse

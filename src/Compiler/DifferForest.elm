module Compiler.DifferForest exposing (backwardClosure, diff, forwardClosure)

import Compiler.Differ exposing (DiffRecord)
import List.Extra


{-| Module `Compiler.DifferForest` is designed to diff lists with an
implicit forest structure (list of trees) defined by a
function `level: p -> Int`. In the resulting `DiffRecord`,
the prefix, suffix, and middle segments all
represent subforests.

To illustrate
the main issue, consider the lists `u` and `v` (below). These
have an indentation structure like an outline for
an article, and so define the structure
of a forest. In the example
below, the leaf `jkl` in the tree with root `def` is
changed to `JKL`.

```text
    u:
    ----
    abc
    def
      ghi
      jkl
      mno
    pqr

    v:
    ----
    abc
    def
      ghi
      JKL
      mno
    pqr
```

In this example the diff record represents the following structure:

```text
    commonPrefix:
    ----
    abc

    middleSegmentInSource:
    ---
    def
      ghi
      jkl
      mno

    middleSegmentInTarget:
    ---
    def
      ghi
      JKL
      mno

    commonSuffix:
    ---
    pqr
```

-}
diff : (p -> p -> Bool) -> (p -> Int) -> List p -> List p -> DiffRecord p
diff eq level u v =
    let
        a =
            commonPrefix eq u v

        b_ =
            commonSuffixAux eq a u v

        la =
            List.length a

        lb =
            List.length b_

        x =
            u |> List.drop la |> dropLast lb

        y =
            -- IMPORTANT: taking y from v, the second argument
            -- ensures that the line numbers in the common suffix
            -- are correct
            v |> List.drop la |> dropLast lb

        b =
            List.drop (List.length a + List.length y) v
    in
    DiffRecord a b x y |> backwardClosure level |> forwardClosure level


backwardClosure : (p -> Int) -> DiffRecord p -> DiffRecord p
backwardClosure level diffRecord =
    let
        n =
            List.length diffRecord.commonPrefix
    in
    case List.head diffRecord.middleSegmentInTarget of
        Nothing ->
            diffRecord

        Just item ->
            if level item > 0 then
                case List.Extra.unconsLast diffRecord.commonPrefix of
                    Nothing ->
                        diffRecord

                    Just ( last, remaining ) ->
                        backwardClosure level (retreat last remaining diffRecord)

            else
                diffRecord


retreat : p -> List p -> DiffRecord p -> DiffRecord p
retreat last remaining diffRecord =
    let
        n =
            List.length diffRecord.commonPrefix
    in
    { diffRecord
        | commonPrefix = remaining
        , middleSegmentInSource = last :: diffRecord.middleSegmentInSource
        , middleSegmentInTarget = last :: diffRecord.middleSegmentInTarget
    }


forwardClosure : (p -> Int) -> DiffRecord p -> DiffRecord p
forwardClosure level diffRecord =
    case List.Extra.uncons diffRecord.commonSuffix of
        Nothing ->
            diffRecord

        Just ( first, remaining ) ->
            if level first == 0 then
                diffRecord

            else
                forwardClosure level (advance first remaining diffRecord)


advance : p -> List p -> DiffRecord p -> DiffRecord p
advance first remaining diffRecord =
    let
        n =
            List.length diffRecord.commonSuffix + List.length diffRecord.middleSegmentInTarget
    in
    { diffRecord
        | commonSuffix = remaining
        , middleSegmentInSource = diffRecord.middleSegmentInSource ++ [ first ]
        , middleSegmentInTarget = diffRecord.middleSegmentInTarget ++ [ first ]
    }


commonPrefix : (p -> p -> Bool) -> List p -> List p -> List p
commonPrefix eq x y =
    if x == [] then
        []

    else if y == [] then
        []

    else
        case ( List.head x, List.head y ) of
            ( Just a, Just b ) ->
                if eq a b then
                    a :: commonPrefix eq (List.drop 1 x) (List.drop 1 y)

                else
                    []

            _ ->
                []


commonSuffixAux : (p -> p -> Bool) -> List p -> List p -> List p -> List p
commonSuffixAux eq commonPrefixInSource x y =
    -- TODO: more efficient implementation!
    let
        n =
            List.length commonPrefixInSource

        xx =
            List.drop n x |> List.reverse

        yy =
            List.drop n y |> List.reverse
    in
    commonPrefix eq xx yy |> List.reverse


dropLast : Int -> List p -> List p
dropLast k x =
    x |> List.reverse |> List.drop k |> List.reverse


takeLast : Int -> List p -> List p
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse

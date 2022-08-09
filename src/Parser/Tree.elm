module Parser.Tree exposing (fromBlocks, forestFromBlocks, Error(..))

{-| This module provides tools for building
a tree from a string or a list of blocks. As noted
in the README.md, a tree
is represented in text as an outline:

     > data = "1\n 2\n 3\n 4\n5\n 6\n 7"

To build a tree from it, we apply the function fromString:

    fromString :
        node
        -> (Block -> data)
        -> String
        -> Result Error (Tree data)


    > fromString "?" .content data
      Tree "0" [
            Tree "1" [Tree "2" [],Tree "3" [], Tree "4" []]
          , Tree "5" [Tree "6" [],Tree "7" []]
      ]

The first argument of fromString is a label for a default node.
The second argument tells how to build a node from a Block.
In the example, we are building a tree with string labels,
so we need a function of type (Block -> String). Recall that

        type alias Block = { indent : Int, content: String }

Therefore

        .content : Block -> String

has the correct type. Here we use the representation of rose trees found in
[elm/rose-tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).

@docs fromString, fromBlocks, forestFromString, forestFromBlocks, Error

-}

-- import Tree.Blocks as Blocks exposing (Block)

import Parser.Forest exposing (Forest)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


{-| -}
type Error
    = EmptyBlocks


type alias State data =
    { blocks : List (Block data)
    , zipper : Zipper data
    , indent : Int
    , indentationChanges : List Int
    , level : Int
    , make : Block data -> data
    , default : Zipper data
    }


type alias Block data =
    { data | indent : Int }


init : data -> (Block data -> data) -> List (Block data) -> Result Error (State data)
init defaultNode makeNode blocks =
    case List.head blocks of
        Nothing ->
            Err EmptyBlocks

        Just rootBlock ->
            Ok
                { blocks = List.drop 1 blocks
                , zipper = Zipper.fromTree <| Tree.tree (makeNode rootBlock) []
                , indent = 0
                , level = 0
                , indentationChanges = []
                , make = makeNode
                , default = Zipper.fromTree (Tree.tree defaultNode [])
                }


{-| -}
fromBlocks : data -> (Block data -> data) -> List (Block data) -> Result Error (Tree data)
fromBlocks defaultNode makeNode blocks =
    case init defaultNode makeNode blocks of
        Err error ->
            Err error

        Ok initialState ->
            Ok <| loop initialState nextStep


{-|

    > Build.fromString "?" .content "1\n 2\n 3"
      Ok (Tree "1" [Tree "2" [],Tree "3" []])

-}
forestFromBlocks : data -> (Block data -> data) -> (data -> Block data) -> List (Block data) -> Result Error (Forest data)
forestFromBlocks defaultNode makeNode renderNode blocks =
    let
        blocks2 : List (Block data)
        blocks2 =
            renderNode defaultNode :: blocks
    in
    fromBlocks defaultNode makeNode blocks2
        |> Result.map Tree.children



-- FUNCTIONAL LOOP


nextStep : State data -> Step (State data) (Tree data)
nextStep state =
    case List.head state.blocks of
        Nothing ->
            Done (Zipper.toTree state.zipper)

        Just block ->
            case compare block.indent state.indent of
                GT ->
                    Loop <| handleGT block.indent block state

                EQ ->
                    Loop <| handleEQ block.indent block state

                LT ->
                    Loop <| handleLT block.indent block state


type Step state data
    = Loop state
    | Done data


loop : state -> (state -> Step state data) -> data
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b



-- HANDLERS


handleEQ : Int -> Block data -> State data -> State data
handleEQ indent block state =
    let
        newTree =
            Tree.tree (state.make block) []
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , zipper = attachAtFocus newTree state.zipper
    }


handleGT : Int -> Block data -> State data -> State data
handleGT indent block state =
    let
        newTree =
            Tree.tree (state.make block) []
    in
    case Zipper.lastChild state.zipper of
        Nothing ->
            -- This is the case for the first data of the tree after the root
            { state
                | blocks = List.drop 1 state.blocks
                , indent = indent
                , level = state.level + 1
                , indentationChanges = pushIndentationChange block.indent state.indentationChanges
                , zipper = attachAtFocus newTree state.zipper
            }

        Just newZipper ->
            { state
                | blocks = List.drop 1 state.blocks
                , indent = indent
                , level = state.level + 1
                , indentationChanges = pushIndentationChange block.indent state.indentationChanges
                , zipper = attachAtFocus newTree newZipper
            }


pushIndentationChange : Int -> List Int -> List Int
pushIndentationChange k ks =
    (k - List.sum ks) :: ks


handleLT : Int -> Block data -> State data -> State data
handleLT indent block state =
    let
        newTree =
            Tree.tree (state.make block) []

        deltaInfo =
            popUntil (state.indent - indent) state.indentationChanges

        deltaLevel =
            deltaInfo.popped
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , level = state.level - deltaLevel
        , indentationChanges = deltaInfo.remaining
        , zipper = attachAtFocus newTree (repeat deltaLevel Zipper.parent state.zipper)
    }


{-|

    > popUntil 5 [1,2,2,2]
      { popped = 3, remaining = [2], sum = 5 }

-}
popUntil : Int -> List Int -> { sum : Int, popped : Int, remaining : List Int }
popUntil goal input =
    popUntilAux goal { sum = 0, popped = 0, remaining = input }


popUntilAux : Int -> { sum : Int, popped : Int, remaining : List Int } -> { sum : Int, popped : Int, remaining : List Int }
popUntilAux goal { sum, popped, remaining } =
    case List.head remaining of
        Nothing ->
            { sum = sum, popped = popped, remaining = remaining }

        Just k ->
            let
                newSum =
                    sum + k
            in
            if newSum < goal then
                popUntilAux goal { sum = newSum, popped = popped + 1, remaining = List.drop 1 remaining }

            else
                { sum = newSum, popped = popped + 1, remaining = List.drop 1 remaining }



-- HELPERS I


attachAtFocus : Tree.Tree data -> Zipper data -> Zipper data
attachAtFocus t z =
    Zipper.replaceTree (appendChild t z) z


appendChild : Tree data -> Zipper data -> Tree data
appendChild t z =
    Tree.appendChild t (Zipper.tree z)



-- HELPERS II


{-|

    Apply f to x n times

-}
repeatM : Int -> (data -> Maybe data) -> Maybe data -> Maybe data
repeatM n f x =
    if n == 0 then
        x

    else
        repeatM (n - 1) f (Maybe.andThen f x)


{-|

    Apply f to x n times

-}
repeat : Int -> (a -> Maybe a) -> a -> a
repeat n f x =
    case repeatM n f (Just x) of
        Nothing ->
            x

        Just y ->
            y

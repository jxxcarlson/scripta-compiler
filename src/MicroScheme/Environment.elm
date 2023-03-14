module MicroScheme.Environment exposing
    ( Environment
    , addSymbolToRoot
    , current
    , currentId
    , initial
    , resolve
    , root
    , setFocus
    )

import MicroScheme.Expr exposing (Expr)
import MicroScheme.Frame as Frame exposing (Frame, FrameId)
import MicroScheme.Init as Init
import Tree exposing (Tree)
import Tree.Zipper exposing (Zipper)


type alias Environment =
    Zipper Frame


resolve : Environment -> Expr -> Expr
resolve env expr =
    -- TODO: search tree
    Frame.resolve (root env) expr


initial =
    Tree.Zipper.fromTree (Tree.singleton Init.rootFrame)


root : Environment -> Frame
root environment =
    current (Tree.Zipper.root environment)


current : Environment -> Frame
current environment =
    Tree.Zipper.label environment


currentId : Environment -> FrameId
currentId environment =
    .id (current environment)


setFocus : FrameId -> Environment -> Maybe Environment
setFocus id environment =
    Tree.Zipper.findFromRoot (\frame -> frame.id == id) environment


replaceRoot : Frame -> Environment -> Environment
replaceRoot frame environment =
    let
        id =
            currentId environment

        newEnvironment_ =
            Tree.Zipper.replaceLabel frame (Tree.Zipper.root environment)
    in
    case setFocus id newEnvironment_ of
        Nothing ->
            environment

        Just newEnvironment ->
            newEnvironment


addSymbolToRoot name expr_ environment =
    let
        newRoot =
            Frame.addSymbol name expr_ (root environment)
    in
    replaceRoot newRoot environment

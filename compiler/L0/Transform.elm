module L0.Transform exposing (transform)

import Either exposing (Either(..))
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))


ordinaryBlock args exprs data =
    ExpressionBlock
        { data
            | blockType = OrdinaryBlock args
            , content = Right exprs
            , args = List.drop 1 args
            , name = List.head args
        }


splitString splitter str =
    if String.contains splitter str then
        String.split splitter str |> List.map String.trim

    else
        String.words str


split exprs =
    case exprs of
        [ Text str m ] ->
            let
                strs =
                    splitString "," str
            in
            List.map (\s -> Text s m) strs

        _ ->
            exprs


{-| The role of function transform is to map a paragraph block
containing a single expression of designated name to
an ordinary block with designated arguments
-}
transform : ExpressionBlock -> ExpressionBlock
transform ((ExpressionBlock data) as block) =
    case data.content of
        Right [ Fun "bibitem" exprs m2 ] ->
            ordinaryBlock [ "bibitem" ] (exprs |> split) data m2

        Right [ Fun "desc" exprs m2 ] ->
            ordinaryBlock [ "desc" ] (exprs |> split) data m2

        _ ->
            block

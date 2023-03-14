module MicroScheme.Numbers exposing (NumberError(..), coerce, roundTo)

import Bool.Extra
import Either exposing (Either(..))
import Maybe.Extra
import MicroScheme.Expr exposing (Expr(..))


type NumberError
    = NotAllNumbers


coerce : List Expr -> Result NumberError (Either (List Int) (List Float))
coerce exprs =
    if allInts exprs then
        Ok (Left (unwrapIntegers exprs))

    else if allFloats exprs then
        Ok (Right (unwrapFloats exprs))

    else if allNumbers exprs then
        Ok (Right (coerceToFloats exprs))

    else
        Err NotAllNumbers


roundTo : Int -> Float -> Float
roundTo n x =
    let
        factor =
            10.0 ^ toFloat n

        raised : Float
        raised =
            round (factor * x) |> toFloat
    in
    raised / factor



-- HELPERS


allFloats : List Expr -> Bool
allFloats exprs =
    let
        isFloat : Expr -> Bool
        isFloat expr =
            case expr of
                F _ ->
                    True

                _ ->
                    False
    in
    List.map isFloat exprs |> Bool.Extra.all


allInts : List Expr -> Bool
allInts exprs =
    let
        isInt : Expr -> Bool
        isInt expr =
            case expr of
                Z _ ->
                    True

                _ ->
                    False
    in
    List.map isInt exprs |> Bool.Extra.all


isNumber : Expr -> Bool
isNumber expr =
    case expr of
        Z _ ->
            True

        F _ ->
            True

        _ ->
            False


allNumbers : List Expr -> Bool
allNumbers exprs =
    exprs |> List.map isNumber |> Bool.Extra.all


unwrapFloats : List Expr -> List Float
unwrapFloats exprs =
    case exprs |> List.map unwrapFloat |> Maybe.Extra.combine of
        Nothing ->
            []

        Just floats ->
            floats


coerceToFloats : List Expr -> List Float
coerceToFloats exprs =
    case exprs |> List.map coerceToFloat |> Maybe.Extra.combine of
        Nothing ->
            []

        Just floats ->
            floats


coerceToFloat : Expr -> Maybe Float
coerceToFloat expr =
    case expr of
        Z n ->
            Just (toFloat n)

        F x ->
            Just x

        _ ->
            Nothing


unwrapFloat : Expr -> Maybe Float
unwrapFloat expr =
    case expr of
        F x ->
            Just x

        _ ->
            Nothing


unwrapIntegers : List Expr -> List Int
unwrapIntegers exprs =
    case exprs |> List.map unwrapInteger |> Maybe.Extra.combine of
        Nothing ->
            []

        Just ints ->
            ints


unwrapInteger : Expr -> Maybe Int
unwrapInteger expr =
    case expr of
        Z n ->
            Just n

        _ ->
            Nothing

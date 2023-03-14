module MicroScheme.Interpreter exposing (State, init, input, runProgram, step)

import MicroScheme.Environment as Environment exposing (Environment)
import MicroScheme.Eval as Eval
import MicroScheme.Expr exposing (Expr(..))
import MicroScheme.Frame as Frame
import MicroScheme.Parser as Parser


type alias State =
    { input : String
    , output : String
    , environment : Environment
    }


init : String -> State
init str =
    { input = str
    , output = ""
    , environment = Environment.initial
    }


input : String -> State -> State
input str state =
    { state | input = String.trim str }


{-|

    TODO: BUG
    > (define (isEven x ) ((remainder x 2 ) = 0 ) )
    ENV: Zipper { after = [], before = [], crumbs = [], focus = Tree { bindings = Dict.fromList [("*",Sym "*"),("+",Sym "+"),("<",Sym "<"),("<=",Sym "<="),("=",Sym "="),(">",Sym ">"),(">=",Sym ">="),("isEven",L [SF Lambda,L [Str "x"],L [L [Sym "remainder",Str "x",Z 2],Sym "=",Z 0]]),("remainder",Sym "remainder"),("roundTo",Sym "roundTo"),("square",L [SF Lambda,L [Str "x"],L [Sym "*",Str "x",Str "x"]])], id = 0 } [] }
    isEven
    > (isEven 2)
    EvalError 0 "Missing case (eval)"

    > runProgram ";" "(define x 5); (* 5 5)"
    "25"

    > runProgram "\n" "(define x 5)\n (* 5 5)"
    "25"

-}
runProgram : String -> String -> String
runProgram separator inputString =
    let
        inputList =
            inputString |> String.split separator |> List.map String.trim

        initialState =
            init ""

        finalState =
            List.foldl (\str state_ -> state_ |> input str |> step) initialState inputList
    in
    finalState.output


{-|

    > s1 = init "(define x 5)"
    { globalFrame = Dict.fromList [], input = "(define x 5)"
    , output = "", symbolTable = Dict.fromList [("*",Sym "*"),("+",Sym "+")] }

    > s2 = step s1
    { globalFrame = Dict.fromList [], input = "(define x 5)"
    , output = "define x : 5", symbolTable = Dict.fromList [("*",Sym "*"),("+",Sym "+"),("x",Z 5)] }

    > s3 = input "(+ x 1)" s2
    { globalFrame = Dict.fromList [], input = "(+ x 1)"
    , output = "define x : 5", symbolTable = Dict.fromList [("*",Sym "*"),("+",Sym "+"),("x",Z 5)] }

    > s4 = step s3
    { globalFrame = Dict.fromList [], input = "(+ x 1)"
    , output = "6", symbolTable = Dict.fromList [("*",Sym "*"),("+",Sym "+"),("x",Z 5)] }

-}
step : State -> State
step state =
    case Parser.parse (Environment.root state.environment) state.input of
        Err err ->
            { state
                | output = "Parse error"
            }

        Ok expr ->
            case expr of
                Define (Str name) body ->
                    { state | environment = Environment.addSymbolToRoot name body state.environment, output = name }

                Define (L ((Str name) :: args)) (L body) ->
                    let
                        newBody =
                            List.map (Frame.resolve (Environment.root state.environment)) body

                        value =
                            Lambda (L args) (L newBody)
                    in
                    { state | environment = Environment.addSymbolToRoot name value state.environment, output = name }

                Define (L ((Str name) :: args)) body ->
                    let
                        newBody =
                            Frame.resolve (Environment.root state.environment) body

                        value =
                            Lambda (L args) newBody
                    in
                    { state | environment = Environment.addSymbolToRoot name value state.environment, output = name }

                _ ->
                    case Eval.eval state.environment expr of
                        Err error ->
                            { state | output = "error: XXX" }

                        Ok value ->
                            { state | output = display value }


display : Expr -> String
display expr =
    case expr of
        Z n ->
            String.fromInt n

        F x ->
            String.fromFloat x

        B b ->
            case b of
                True ->
                    "True"

                False ->
                    "False"

        Str s ->
            s

        Sym s ->
            s

        u ->
            "Unprocessable expression"

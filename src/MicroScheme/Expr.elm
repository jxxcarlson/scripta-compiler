module MicroScheme.Expr exposing (Expr(..))


type Expr
    = Z Int
    | F Float
    | B Bool
    | Str String
    | Sym String
    | L (List Expr)
    | Pair Expr Expr
    | Lambda Expr Expr
    | Define Expr Expr
    | If Expr Expr Expr

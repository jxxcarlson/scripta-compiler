module MicroScheme.Error exposing (..)

import MicroScheme.Frame as Frame


type EvalError
    = EvalError Int String
    | FR Frame.FrameError

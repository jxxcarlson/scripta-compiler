# Common Code

## Functional loops

Functional loops are used throughout the compiler, and in
particular in the L0 parser.  The key element of
such a loop is a driver function
`f : state -> Step state a` that does some computation
on the state and returns either a value of type `Done a`
or a value of type `Loop state`.   In the first case
the loop terminates with the indicated value.  In the
second case it runs again with the new state value.˜

```
-- Parser.Helpers
type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b
```


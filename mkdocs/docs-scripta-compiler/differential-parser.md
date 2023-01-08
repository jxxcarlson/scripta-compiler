# Differential Parser

Differential parsing is an optimization used to speed up
recompilation of large inputs.  Since parsing is the most
expensive part of the compiler pipeline, the idea is
to only re-parse what has been changed after an edit.

## Differ

We use an extremely primitive strategy.
Let u and v be two lists of things of type `q`. Write them as
u = axb, v = ayb, where a is the greatest common prefix
and b is the greatest common suffix. 
Return DiffRecord a b x y.  This operation is carried
out by 

```text
-- Compiler.Differ
diff : List p -> List p -> DiffRecord p
```

where

```text
-- Compiler.Differ
type alias DiffRecord p =
    { commonPrefix : List p
    , commonSuffix : List p
    , middleSegmentInSource : List p
    , middleSegmentInTarget : List p
    }
```

A `DiffRecord` can be used to transform a list 
using function `differentialTransform` and 
a function `transform: p -> q`.

```text
-- Compiler.Differ
differentialTransform : (p -> q) -> DiffRecord p -> List q -> List q
```
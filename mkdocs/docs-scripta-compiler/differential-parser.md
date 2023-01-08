# Differential Parser

Differential parsing is an optimization used to speed up
recompilation of large inputs.  Since parsing is the most
expensive part of the compiler pipeline, the idea is
to only re-parse what has been changed after an edit.

## Differ

We use an extremely primitive strategy.
Let u and v be two lists of things of type `p`. Write them as
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

## DifferForest

Module `Compiler.DifferForest` is designed to diff lists with an 
implicit forest structure (list of trees) defined by a 
function `level: p -> Int`. In the resulting `DiffRecord`,
the prefix, suffix, and middle segments all 
represent subforests.

To illustrate
the main issue, consider the lists `u` and `v` (below). These
have an indentation structure like an outline for
an article, and so define the structure
of a forest. Here the leaf `jkl` in the tree with root `def` is
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

The resuting diff record represents the following structure:

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

## Differential Parsing in Scripta

In Scripta, differential parsing takes place
at the level of primitive blocks, as indicated in
the [flowchart](/docs-scripta-compiler/overview#flowchart).
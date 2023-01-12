# Rendering

```
render : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
```
of module `Block.Render`.

## TeX Macros and L0 Function Elements 

Recall that running text in MicroLaTeX and L0
is represented by a common `List Expr` type,
where

```text
type Expr
    = Fun String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta
```

Thus both the LaTeX `\italic{stuff}`
and the L0 `[italic stuff]` are represented by
`Fun "italic" [Text "stuff"]`.  Values of 
type `Expr` are rendered to Html by

Expressions are rendered by the function
`render`, the sole export of module `Render.Elm`:

```text
-- Render.Elm
render : 
    Int
    -> Accumulator
    -> Settings 
    -> Expr 
    -> Element MarkupMsg
render generation acc settings expr = ...
```

Expressions of type `Fun ...` are handled
by a dictionary that maps function names
to rendering functions:

```text
markupDict : 
  Dict 
    String 
    (Int -> Accumulator -> Settings  
      -> List Expr -> Element MarkupMsg)
```

The entries in that dictionary are listed 
below

### Function Elements

#### Style

```text
i, italic, textit, emph
b, bold, strong, textbf
bi, boldItalic
strike
underline
large
```

#### Color

```text
red
blue
green
pink
magenta
violet
highlight
gray
errorHighlight
```

#### Unclassified and ???

```text
var
lambda
group
skip
table
image
```


#### Formatting

```text
vspace
par
```


#### Reference

```text
bibitem
ref
reflink
eqref
cite
term
term_
footnote
label
```

#### Document

```text
hide
author
date
today
comment
title
setcounter
abstract
```


#### Links

```text
link
href
ilink
ulink
cslink
```

#### Scripta

```text
tags
```


#### Character, LaTeX

```text
dollarSign
dollar
brackets
rb
lb
bt
ds
bs
texarg
backTick
underscore
mdash
ndash
```

### Verbatim Elements

Under construction

```text
-- Render.Block
renderVerbatimBlock : 
   Int -> Accumulator -> Settings -> 
   ExpressionBlock -> Element MarkupMsg
```

## Verbatim Blocks


```text
-- Render.Block and Parser.PrimitiveLaTeXBlock
-- 21 blocks
equation
aligned
math
code
verbatim
verse
mathmacros
textmacros
tabular
hide
docinfo
datatable
chart
svg
quiver
image
tikz
load-files
include
iframe
```

Under construction

# Data Structures

The primary data structures of the parser are

1.  Primitive Blocks (`PrimitiveBlock`)
2. Expressions (`Expr`)
3. Expression blocks (`ExpressionBlock`)
4. Accumulator (`Accumulator`)

The parser first breaks source text into primitive
blocks (1), then maps the internal language parser over 
a list of primitive blocks to produce expression
blocks (3).  Finally, the function
[transformAccumulate](#accumulator) is mapped over the list 
of expression blocks, simultaneously collecting
information such as cross-references and updating
the list of expression blocks with that information


## Primitive Blocks

Blocks have the rather elaborate type listed below.  
This complex structure is necessary for interactive
real-time editing with error recovery.  By real-time we mean
that the rendered text is updated "instantaneously"
in the browser as the user types in text.  By interactive,
we mean (for example) that click on the rendered text
brings the associated source text into view while
simultaneously highlighting it.  In addition, if the
user selects a block of source text and presses
ctrl-S (S for 'sync'), the corresponding rendered text
is brought into view and highlighted.





In the case of L0 and XMarkdown, a primitive
block is defined by

```
-- Parser.PrimitiveBlock
type alias PrimitiveBlock =
{ indent : Int
, lineNumber : Int
, position : Int
, content : List String
, name : Maybe String
, args : List String
, properties : Dict String String
, sourceText : String
, blockType : PrimitiveBlockType
, error : Maybe { error : String }
}
```

In the case of
MicroLaTeX, there are two additional fields,
`level: Int` and `status: Status`.


## Expressions

```text
-- Parser.Expr
type Expr
    = Fun String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta
```

## Expression Blocks

```
-- Parser.Block
type ExpressionBlock
    = ExpressionBlock
        { name : Maybe String
        , args : List String
        , properties : Dict String String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , id : String
        , tag : String
        , blockType : BlockType
        , content : Either String (List Expr)
        , messages : List String
        , sourceText : String
        , error : Maybe { error : String }
        }
```


## Edit records

**Module:** Compiler.AbstractDifferentialCompiler

To manage differential compilation, one uses
_EditRecords_.  The main idea is to keep track 
of the current list of primitive blocks (`List chunk`)
and the current parse structure (`tree`).  When an
edit is made, the source is parsed into primitive
blocks and the new and old lists are compared, producing
a _diff record_.  ... TO BE CONTINUED

```
type alias EditRecord chunk parsedChunk accumulator =
    { chunks : List chunk
    , parsed : List parsedChunk
    , tree : List (Tree parsedChunk)
    , accumulator : accumulator
    , lang : Language
    , messages : List String
    , initialData : InitialData
    }
```

## Diff Records

**Module:** Compiler.Differ

Let u and v be two lists of things of type `p`. Write them as
u = axb, v = ayb, where a is the greatest common prefix
and b is the greatest common suffix. Return DiffRecord a b x y.
The type parameter can be anything: characters, strings, 
primitive blocks, etc.  In the example below, `p = Char`.

```text
type alias DiffRecord p =
    { commonPrefix : List p
    , commonSuffix : List p
    , middleSegmentInSource : List p
    , middleSegmentInTarget : List p
    }
```

**Example**

```text
> import Compiler.Differ exposing(..)
> a = "abcxyzdef" |> String.split ""
["a","b","c","x","y","z","d","e","f"]
    : List String
> b = "abcXYZdef" |> String.split ""
["a","b","c","X","Y","Z","d","e","f"]
    : List String
> diff a b
{  commonPrefix = ["a","b","c"]
 , commonSuffix = ["d","e","f"],
 , middleSegmentInSource = ["x","y","z"]
 , middleSegmentInTarget = ["X","Y","Z"] }
```

## Diffing a Forest

Module `Compiler.DifferForest` is designed to diff lists with an
implicit forest structure (list of trees) defined by a
function `level: p -> Int`. In the resulting `DiffRecord`,
the prefix, suffix, and middle segments all
represent subforests.  The `diff` function in this 
module is used to diff primitive blocks in the compiler.

```text
diff : (p -> p -> Bool) 
       -> (p -> Int) 
       -> List p 
       -> List p 
       -> DiffRecord p
diff eq level u v =
```

The function `eq: p -> p -> Bool` tests for equality
... XX: more detail.  In the case of primitive blocks ...
important that line numbers be properly computed
in the common suffix ...

To illustrate
the main issue, consider the lists `u` and `v` (below). These
have an indentation structure like an outline for
an article, and so define the structure
of a forest. In the example
below, the leaf `jkl` in the tree with root `def` is
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

In this example the diff record represents the following structure:

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



## Accumulator

**Module:** `Compiler.Acc`

The `Accumulator` type
is used to collect, build up, 
and apply auxiliary information about the text.
For example, the `headingIndex` stores the current
section number, where "current" refers to the 
the current section of the document as the 
accumulator-building function walks through
the current parse forest.  As it does, it simultaneously
(1) updates the accumulator and (2) applies new "patches" 
of the accumulator to the parse forest.  

This process is managed by
function `transformAccumulateTree` which is called every time 
the parser pipeline runs.  For this reason one does not have
to recompile to have up-to-date cross-references, etc., as
in standard LaTeX.


```text
transformAccumulate : 
        InitialAccumulatorData 
        -> Forest ExpressionBlock 
        -> ( Accumulator, Forest ExpressionBlock )
```

Type definition:


```text
-- Compiler.Acc
type alias Accumulator =
    { language : Language
    , headingIndex : Vector
    , documentIndex : Vector
    , counter : Dict String Int
    , blockCounter : Int
    , itemVector : Vector -- Used for section numbering
    , numberedItemDict : Dict String { level : Int, index : Int }
    , numberedBlockNames : List String
    , inList : Bool
    , reference : Dict String { id : String, numRef : String }
    , terms : Dict String TermLoc
    , footnotes : Dict String TermLoc
    , footnoteNumbers : Dict String Int
    , mathMacroDict : Parser.MathMacro.MathMacroDict
    , textMacroDict : Dict String Macro
    , keyValueDict : Dict String String
    , qAndAList : List ( String, String )
    , qAndADict : Dict String String
    }
```


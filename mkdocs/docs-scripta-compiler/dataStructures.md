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

### Example (L0)

Consider the block 

```text
| theorem (Magnus Axelsson) note:this is a joke temperature:22
If $a = b$ then $b = a$.
```

There "(Magnus Axelsson) note:this is a joke temperature:22"
is the argument string.  This string is a sequences
of words.  Normal words do not contain the ":" character.
Words of the form *a:b*, where *a* is normal are key-value
words, representing key-value pairs.  Normal words must
precede key-value words.  The former make up the 
argument list, whereas the latter define the entries
of the property dictionary.  Thus we have


```text
args = ["(Magnus", "Axellson)"]
properties = Dict.fromList [("note", "this is a joke"), ("temperature", "22")]
```

Arguments and properties are treated the same for 
verbatim blocks as for ordinary blocks.  

### Example (MicroLaTeX)

Below is a LaTeX environment with an LaTeX optional argument.  In this 
case the args list is empty and their are three key-value pairs
in the properties dictiononary

```text
\begin{theorem}[foo:1, hoho:a b c, bar:2]
If $a = b$ then $b = a$
\end{theorem}
```

```text
args = []
properties = Dict.fromList [("bar", "2"), ("foo", "1"), ("hoho", "a b c"]
```

At the moment, environments with normal arguemnts are not handled, e.g.,


```text
\begin{theorem}{X}{Y}[foo:1, hoho:a b c, bar:2]
If $a = b$ then $b = a$
\end{theorem}
```

This should produce `args = ["X", "Y"]`


### Coercion and Pseudoblocks


Certain constructs such as `\image{https://yadayada.org/sparrow.jpg width:300}` or
`\item foo bar ...` look to be syntactically part of 
the surface internal language of MicroLaTeX.  However, 
in source text they alway appear with an empty line above and 
below and so are parsed as an anonymous (paragraph)
block.  They must be coerced into being an ordinary or 
verbatim block, e.g.,

```text
{ name = "image"
, blockType = PBVerbatim
, properties = Dict.fromList [("width", "200")]
, content = ["image{https://yadayada.org/sparrow.jpg"]
...
}
```

or 

```text
{ name = "image"
, blockType = PBOrdinary
, content = ["foo bar ..."]
...
}
```

This coercion is accomplished by functions in module `MicroLaTeX.Parser.Transform`, 
e.g.,


```text
-- module MicroLaTeX.Parser.Transform
handlePseudoBlockWithContent : String -> Maybe String -> PrimitiveBlock -> PrimitiveBlock
handlePseudoBlockWithContent name maybeArg block = ...

handleImage : PrimitiveBlock -> PrimitiveBlock
```

The names of these elements must appear in the `pseudoBlockNamesWithContent`
list:

```text
-- module MicroLaTeX.Parser.Transform
pseudoBlockNamesWithContent =
    [ "title", "section", "subsection", "subsubsection"
    , "subheading", "setcounter", "contents", "endnotes", "image" ]

```


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
a _diff record_ which shows which blocks have been
changed.  The changed blocks are parsed,
then incorporated into the current list of 
expression blocks (`List parsedChunk`).  


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

Orchestration of the process outlined above
is conducted by the `update` function listed
below.  

```text
-- Compiler.AbstractDifferentialParser
update :
    UpdateFunctions chunk parsedChunk acc
    -> String
    -> EditRecord chunk parsedChunk acc
    -> EditRecord chunk parsedChunk acc
```
The first argument is a record that holds the 
functions needed in any concrete instantiation
of the `update` function, e.g, the field

```text
setLineNumber : Int -> parsedChunk -> parsedChunk
```

used in renumbering parsed chunks.

XX: more on keeping the line numbers up-to-data.


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

**Module:** `Compiler.DifferForest`

The `diff` function in this module 
is designed to diff lists with an
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
  
XX, Expand on this, explain it:

            -- IMPORTANT: taking y from v, the second argument
            -- ensures that the line numbers in the common suffix
            -- are correct

## Accumulator

**Module:** `Compiler.Acc`

The `Accumulator` is a data structure that holds information
such as `counter : Dict String Int` that holds counter
of various kinds, e.g., there is a key for "figure" with
value whatever the current figure number is.  The function
`transformAccumulate` walks through the syntax forest simultaneously
building up the `Accumulator` data structure and
transforming the current block.  For example, when it
encounters an image, iframe, quiver, or chart block, it advances
the figure counter and inserts an entry in the property dictionary
of the block with key "figure" and value the current counter.
In this way, the rendering function has at hand all
the information needed to render the block with a label
like "Figure 23."

```text
-- module Compiler.Acc
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
### Traversal function

```text
-- module Compiler.Acc
transformAccumulate : InitialAccumulatorData -> Forest ExpressionBlock -> ( Accumulator, Forest ExpressionBlock )
transformAccumulate data forest =
```

### Block updater

```text
-- module Compiler.Acc
transformBlock : Accumulator -> ExpressionBlock -> ExpressionBlock
```
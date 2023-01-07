# Data Structures

The primary data structures of the parser are

1.  Primitive Blocks (`PrimitiveBlock`)
2. Expressions (`Expr`)
3. Expression blocks (`ExpressionBlock`)
4. Accumulator (`Accumlator`)

The parser first breaks source text into primitive
blocks (1), then maps the internal language parser over 
a list of primitive blocks to produce expression
blocks (2).


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


```
-- Compiler.AbstractDifferentialCompiler
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

## Accumulator

The `Accumulator` type, defined in module `Compiler.Acc`,
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
transformAccumulateTree : Tree ExpressionBlock -> Accumulator -> ( Accumulator, Tree ExpressionBlock )
transformAccumulateTree tree acc =
    Tree.mapAccumulate transformAccumulateBlock acc tree
```

where the function `transformAccumulateBlock` carries out the 
per-block step:

```text
transformAccumulateBlock : Accumulator -> ExpressionBlock -> ( Accumulator, ExpressionBlock )
```

This function in turn calls upon functions `updateAccumulator` for (1) and 
function `transformBlock` for (2):

```text
updateAccumulator : ExpressionBlock -> Accumulator -> Accumulator
```

```text
transformBlock : Accumulator -> ExpressionBlock -> ExpressionBlock
```


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


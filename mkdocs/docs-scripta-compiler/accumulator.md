# Accumulator

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

## Data structure

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
## Traversal function

```text
-- module Compiler.Acc
transformAccumulate : InitialAccumulatorData -> Forest ExpressionBlock -> ( Accumulator, Forest ExpressionBlock )
transformAccumulate data forest =
```

## Block updater

```text
-- module Compiler.Acc
transformBlock : Accumulator -> ExpressionBlock -> ExpressionBlock
```
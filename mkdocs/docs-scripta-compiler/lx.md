# MicroLaTeX

The MicroLaTeX parser first transforms source
into a list of 
[primitive LaTeX blocks](#primite-blocks) using
a [shift-reduce parser with error recovery](#primitive-block-parser).
It then maps the parser for the internal
language over this list, into a list of expression blocks.


## Data Structures 

### Primitive Blocks

A primitve LaTeX block is a 13-field record as displayed
below.  


```
-- Parser.PrimitiveLaTeXBlock
type alias PrimitiveLaTeXBlock =
    { indent : Int
    , lineNumber : Int
    , position : Int
    , level : Int
    , content : List String
    , firstLine : String
    , name : Maybe String
    , args : List String
    , properties : Dict String String
    , sourceText : String
    , blockType : PrimitiveBlockType
    , status : Status
    , error : Maybe PrimitiveBlockError
    }

```


1. When the source text is parsed into a list of 
blocks, it is grouped into lists of strings in the 
`content` field. 


2. The content is also stored as a string
in the `sourceText` field.  This is to facilitate 
synchronization of source text and rendered text.
The source text field is carried into the final
syntax tree (forest of expression blocks).  Consequently,
if a piece source text is selected, the syntax tree
can can be searched, the matching element can be
located and then used to highlight the corresponding
part of the rendered text.

3. The `indent` field is the number of spaces of indentation
of the first line of the block; `lineNumber` is the
line number of the first line of the text in the source
string; `position` is its character position in that 
string.  The `level` field is the depth of the block
in the eventual tree structure.

4. Blocks may be un-named, as in the case of paragraph,
or named, in the case of a LaTeX environment.  This
information is stored in the `name` field.  For example,
the name of the block `\begin{theorem} ... \end{theorem}`
is `Just "theorem"`

5. The `firstline` field is the first line of a block,
i.e., its header. If the header of a 
block is "\begin{theorem}[Pythagoras]", then
its name is "theorem" and `args` is the list `["Pythagoras]`.
If we had `"\begin{theorem}[Pythagoras, foo:bar]"` (XX)
then `args` is still ["Pythagoras"], and `properties` is
a dictionary with one key, "foo", whose value is "bar". 
Thus `args` is a list of unnamed args and `properties`
is a dictionary of key-value pairs derived from the named
args. (XX: improve this discussion)

6. The `blockType` field has type
    
    ```
    type PrimitiveBlockType
        = PBVerbatim
        | PBOrdinary
        | PBParagraph
    ```
   
    It describes the type of block — unnamed, environment
    like "theorem" in which the body of the block is parsed,
    or environment like "equation" where it is passed 
    verbatim to the renderer.

7. The `status` field has type 

    ```
    type Status
        = Finished
        | Started
        | Filled
    ```
   
    It is used by the primitive block parser and 
    is needed to handle nested blocks.






## Primitive Block Parser

The parser is defined in 
module `Parser.PrimitiveLaTeXBlock`.
Lists of lines of text are parsed into lists
of primitive blocks by the function

```
-- Parser.PrimitiveLaTeXBlock
parse : List String -> List PrimitiveLaTeXBlock
parse lines =
    lines |> parseLoop |> .blocks
```

The strategy is to examine each line in turn,
committing the current block if its mathching end 
tag is found, otherwise pushing it onto a stack of blocks.
All blocks are moved from the stack to the committed list
when the "root" or first block on the stack 
as well as all of its children are closed. If the stack is nonempty after all
blocks have been consumed, there has been a syntax error, and
so the error recovery procedure is invoked. The strategy is to close 
the root block on the stack with an
error message, then reparse after skipping over the root block.
Error recovery always
terminates and provides an indication of the nature of the error.

### Data structure

```text
-- Parser.PrimtiiveLaTeXBlock
-- 14 fields
type alias State =
    { blocks : List PrimitiveLaTeXBlock
    , stack : List PrimitiveLaTeXBlock
    , holdingStack : List PrimitiveLaTeXBlock
    , labelStack : List Label
    , lines : List String
    , sourceText : String
    , firstBlockLine : Int
    , indent : Int
    , level : Int
    , lineNumber : Int
    , position : Int
    , verbatimClassifier : Maybe Classification
    , count : Int
    , label : String
    }
```

where

```text
type alias Label =
    { classification : ClassifyBlock.Classification
    , level : Int
    , status : Status
    , lineNumber : Int
    }
```

1. The `blocks` field holds the committed blocks — the eventual output
of the parser.
2. The `stack` field holds

### Main parsing functions


```text
parse : List String -> List PrimitiveLaTeXBlock
parse lines =
    lines |> parseLoop |> .blocks
```

and

```text
parseLoop : List String -> ParserOutput
parseLoop lines =
    loop (init lines) nextStep |> finalize
```

where

```text
type alias ParserOutput =
    { blocks : List PrimitiveLaTeXBlock
    , stack : List PrimitiveLaTeXBlock
    , holdingStack : List PrimitiveLaTeXBlock }
```

### The nextStep function

This is the driver function for the parser's
[functional loop](/docs-scripta-compiler/common-code#functional-loops/).
It operates as follows:

```text
  - Increment state.lineNumber.

  - If the input (state.lines) has been consumed and
        - the stack is empty, return Done state
        - the stack is non empty, return recoverFromError state
  - Let the current raw line be the string at index state.lineNumber of state.lines.
  - Classify the raw line, a value of type Classification:

        type Classification
            = CBeginBlock String
            | CEndBlock String
            | CSpecialBlock LXSpecial
            | CMathBlockDelim
            | CVerbatimBlockDelim
            | CPlainText
            | CEmpty

  - Invoke a handler based on the classification that returns a value
    of type Step State State
```




## Transform


**Module MicroLaTeX.Parser.Transform**

The purpose of this function is to transform a primitive block
like the one coming from a single-line paragraph with text
"\section{Intro}" to an ordinary (blockType PBOrdinaryBlock)
block with name "section", args ["1"], and content ["Introduction"].
This is to coerce parsed MiniLaTeX source to our standard model.

## Verbatim Blocks

If a block is to be treated as a verbatim block,
its name must appear in 

```text
Parser.LaTeXBlock.verbatimBlockNames : List String
```



**Note.** What's the deal with `Parser.Common.verbatimBlockNames`?


```text
verbatimBlockNames =
    [ "equation"
    , "aligned"
    , "math"
    , "code"
    , "verbatim"
    , "verse"
    , "mathmacros"
    , "textmacros"
    , "tabular"
    , "hide"
    , "docinfo"
    , "datatable"
    , "chart"
    , "svg"
    , "quiver"
    , "image"
    , "tikz"
    , "load-files"
    , "include"
    , "iframe"
    ]

```

## Tests

Test parsing of text to a list of primitive blocks:

```text
-- MicroLaTeXParserTest
primitiveBlockRoundTripTest "nested environments" text1
```

Test the internal language:

```
-- MicroLaTeXParserTest
roundTrip1 "\\blue{\\italic{abc \\strong{def}}}"
```

Test coercion of MicroLaTeX macros to blocks:

```text
-- TransformLaTeXTest
test_ "tags" (toL0 [ "\\tags{AAA}" ]) [ "| tags AAA " ]
```

```text
-- TransformTest
test_ "transform, args"
    (toPrimitiveBlocks "\n\n\\section{Intro}\n\n" |> List.map transform |> List.map .args)
    [ [ "1" ] ]
```

where

```text
toPrimitiveBlocks = 
  Markup.toPrimitiveBlocks MicroLaTeXLang

```
## Command line tools


The `./CLI` folder contains various CLI tools for testing
and benchmarking.  All use Albert Dahlin's
[elm/posix](https://package.elm-lang.org/packages/albertdahlin/elm-posix/latest/)
package and can be run using velociraptor (command: `vr`).
Some examples:

- vr lxparse lxtest/a1.txt

- vr rt foo.txt

- vr bench init 100 bench/harmonic.tex



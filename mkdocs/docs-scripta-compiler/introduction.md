# Introduction

The Scripta compiler translates source text written
in a markup language to an Elm representation of Html.



## Markup Languages

The languages supported by Scripta are

- L0
- microLaTeX
- XMarkdown

## Blocks

The text of these markup languages 
should be thought of as 
structured into blocks, the content of which 
is written in an _internal language_. 
For example, in microLaTeX, one might have the text
below.  There are seven blocks, each of which 
is separated from its neighbor by an empty line.
The first block is a paragraph; its content consists
of plain text followed by the TeX macro expression 
`\italic{prime}` followed by more plain text.

```text
Let's talk about \italic{prime} numbers.

\begin{theorem}
There are infinitely many primes $p$, and in fact
there are infinitely many primes 

\begin{equation}
p \equiv 1 \ \text{mod}\ 4
\end{equation}

and also

\begin{equation}
p \equiv 1 \ \text{mod}\ 8
\end{equation}

and so on.
\end{theorem}

The first paragraph of the theorem was known to Euclid. 
```
The body of the theorem block consists of six
blocks — the three paragraph blocks `Let's talk ...`,
`and also`, and `and so on`. There also the two
equation blocks.  The blocks in the body of the 
theorem block constitute the \italic{children} of the
block.  It is the job of the parser to (1) discover
the forest structure, and (2) to parse the content
of the blocks.  

Note that we can visualize the block structure
as an outline, as below.  

```text
PARAGRAPH

THEOREM

  PARAGRPH
  
  EQUATION
  
  EQUATION
  
  PARAGRAPH
  
PARAGRAM
```

In some languages, e.g.
L0 and Markdown, the block structure is literally
given by the "outline" structure, that is, by
indentation.  Below is our example rewritten in L0:

```text
Let's talk about [italic prime] numbers.

| theorem
    There are infinitely many primes $p$, and in fact
    there are infinitely many primes 
    
    || equation
    p \equiv 1 \ \text{mod}\ 4
    
    and also
    
    || equation
    p \equiv 1 \ \text{mod}\ 8
    
    and so on.

The first paragraph of the theorem was known to Euclid. 
```

Note that an outline is fully equivalent to a tree:

```text
|-- PARAGRAPH
|-- THEOREM
    |- PARAGRAPH
    |- EQUATION
    |- EQUATION
    |- PARAGRAPH
|- PARAGRAPH
```


## Internal Language

While the surface syntax in L0, microLaTeX and XMarkdown
depends on the language, the abstract syntax is the
same for all tree.  Indeed, text in the internal 
language always parses to `Either String (List Expr)`,
where

```elm
type Expr
    = Fun String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta
```

## Block Definition




In the case of L0 and XMarkdown, a primitive
block is defined by

```elm
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




# KaTeXExample

In KaTeXExample, we show how to compile 
a very simple language, "SimpleMarkup," 
into Html, rendering it in the browser
using KaTeX.  SimpleMarkup has just three elements: text, inline
math, and display math, as in the example in the next section.

To run the code from the folder `KaTeXExample`, 
say `sh make.sh` and then point your browser at 
`./assets/index.html`

## SimpleMarkup

Here is an example that tells you everything you 
need to know about SimpleMarkup:

```text
Pythagoras says that $a^2 + b^2 = c^2$. In calculus class, we all learned that

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$
```

More formally, text in this language is parsed into
a list of `Expr`, where

```text
type Expr
    = Text String
    | InlineMath String
    | DisplayMath String
```

Parsing is handled by 

```text
parse : String -> Maybe (List Expr)
```

in module `SimpleMarkup`.  Rendering is handled by the function

```text
render : Expr -> Html msg
render expr =
    case expr of
        Text str ->
            Html.text str

        InlineMath str ->
            KaTeX.inline str

        DisplayMath str ->
            KaTeX.display 500 str
```

The functions of module `KaTeX` handle rendering of 
math formulas.  This is done via the `math-text` custom element
which is defined in file `assets/katex.js`.

This is a bare-bones example meant solely to illustrate
basic principles.
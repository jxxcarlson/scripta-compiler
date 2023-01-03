module Data exposing (harmonicOscillator, initialText)

{- This  app shows how to use the Scripta conpiler for microLaTeX.  It does
   not use an
-}


initialText =
    """
You can edit the text below at will.

\\begin{equation}
a^2 + b^2 = c^2
\\end{equation}

In calculus we learn that

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

Note the format of a displayed equation:
blank lines above and below, the double
dollar signs at the left margin, each
on a separate line. This is an example
of a \\italic{block}

\\begin{theorem}
There are infinitely many prime numbers $p \\equiv 1\\ mod\\ 4$
\\end{theorem}

\\image{https://natureconservancy-h.assetsadobe.com/is/image/content/dam/tnc/nature/en/photos/AmericanGoldfinch_MattWilliams_4000x2200.jpg?crop=0%2C0%2C4000%2C2200&wid=4000&hei=2200&scl=1.0}

The format of a simple image is `\\image{URL}`.

"""

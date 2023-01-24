module Data exposing (initialText)

{- This  app shows how to use the Scripta compiler for microLaTeX.  It does
   not use an
-}


initialText2 =
    """
\\begin{theorem}
abc

"""


initialText =
    """
% This is a comment

\\title{Scripta Compiler: Example App}

\\contents

\\section{Intro}

The Scripta compiler renders microLaTeX text on the web in real time.
It is still under development,  but nonetheless quite serviceable.  Questions and
comments are welcome.

Give Scripta a try: you can edit the text below at will. Or delete it and
start from scratch.  Do look at the format, e.g., the 
displayed math formula  below with the usual \\dollar{}\\dollar{} . MicroLaTeX
is \\textit{block-structured}, meaning in particular that blocks, e.g., 
LaTeX environments need a blank line above and below.

This simple app does not use the advanced editing capabilities
of the Scripta compiler, e.g., the synchronization of source and rendered 
text used in the web app \\link{Scripta.io https:scripta.io}.

For a longer example of a microLaTeX document, take a look at
this \\link{example https://scripta.io/s/jxxcarlson:wave-packets-dispersion}.

\\section{Contact}


\\item Elm Slack: jxxcarlson

\\item Github: jxxcarlson

\\item twitter: @epsilon2718

\\item email: jxxcarlson, gmail

\\section{Mathematics}

\\begin{equation}
\\label{pythagoras}
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


\\section{Images}

\\image{https://natureconservancy-h.assetsadobe.com/is/image/content/dam/tnc/nature/en/photos/AmericanGoldfinch_MattWilliams_4000x2200.jpg?crop=0%2C0%2C4000%2C2200&wid=4000&hei=2200&scl=1.0}

The format of a simple image is `\\image{URL}`.

\\section{Notes}

\\item Click on a section heading to jump back to the table of contents.
Click on an entry in the table of contents to go to the corresponding
section.

\\item The Scripta compiler is open source software.  See the
\\link{gitbub repo https://github.com/jxxcarlson/scripta-compiler}.

"""

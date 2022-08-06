module Text exposing (info, l0Demo, microLaTeXDemo, xMarkdown)


info =
    """

| title
About the Scripta compiler

| contents

[tags jxxcarlson:about-the-scripta-compiler]

| runninghead
[link Scripta.io https://scripta.io]


| section 1 -
What it is

The Scripta compiler transforms source text to HTML, where
the source text is one of the following markup languages:

| item
L0 — an experimental language with syntax inspired by Lisp.
Can render LaTeX-style
mathematical text.  This document is written in L0.

| item
MicroLaTeX — a cousin of LaTeX.  Source text can be exported
to standard LaTeX

| item
XMarkdown — a cousin of Markdown.  Can render LaTeX-style
mathematical text.


The Scripta compiler features real-time, fault-tolerant
parsing and rendering, and so is suitable for an interactive
editing system in which (a) changes to the source text
are rendered "instantly," that is, with no perceptible delay,
and (b) syntax errors are handled gracefully, marked as such
in the rendered text, and with the following text rendered
properly to the greatest extent possible.


| section 1 -
Open source

The Scripta compiler is open-source, and can be found at
[link github.com/jxxcarlson/scripta-compiler  https://github.com/jxxcarlson/scripta-compiler].  In the Example
folder, you will find a small demo app.  It is hosted online
at [link Github https://jxxcarlson.github.io/app/scripta-compiler-demo/assets/index.html].

The Scripta compiler is used to power
[link Scripta.io https://scripta.io].  It features
interactive editing, a searchable store of documents,
and facilities for collaboration and web publishing.


| section 1 -
Code

If you are interested in looking at the code, there are two
good places to start. The first is `compiler/Scripta/API.elm`.
The second is the folder `compiler/L0/Parser/` especially
the file `compiler/L0/Parser/Expression`.  The latter
is the shift-reduce parser used for L0, the simplest
of the three markup languages considered.

A notable feature of the Scripta compiler is that
all three markup languages use a common expression
type and parse to a common type (a list of syntax trees)

|| code
type Expr
    = Fun String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta

The three variants of this type align with the three
syntactic elements of `L0`:

| item
Function elements, e.g. `[italic This is italic text]`,
which are bounded on left and right by brackets.

| item
Stretches of pure text,

| item
Verbatim elements, which are bounded by
dollar signs or by backtics, for inline
mathematical text and inline code,
respectively.


| section 1 -
Status and Roadmap

The Scripta compiler is serviceable — I've used to to write
[link these class notes https://scripta.io/s/jxxcarlson:wave-packets-dispersion], for example.
That said, there is still a great deal to be done. Please send bug reports,
feature requests, and comments in general to me at jxxcarlson (gmail).
I am on the Elm Slack and Github as jxxcarlson and on Twitter as @epsilon2718.

"""


microLaTeXDemo =
    """
\\title{Demo (MicroLaTeX)}

| banner
\\link{Scripta.io https://scripta.io}

\\contents

\\section{Images}

\\image{https://see.news/wp-content/uploads/2020/12/UK_wildbirds-01-robin.jpg}

\\section{Math}

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

\\strong{Tip:} Click on a section title to go back to the table of contents.
"""


l0Demo =
    """
| title
Demo (L0)

| banner
[link Scripta.io https://scripta.io]

| contents

| section 1
Images

|| hide
[image https://nas-national-prod.s3.amazonaws.com/styles/hero_image/s3/web_h_apa_2016-a1_2474_8_cedar-waxwing_peter_brannon_kk_female.jpg?itok=VdeVVmGA]

[image https://www.birdsandblooms.com/wp-content/uploads/2018/10/BNBbyc18_patricia-warren.jpg width:400]


| section 1
Math

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

[bold Tip:] Click on a section title to go back to the table of contents.

"""


xMarkdown =
    """
| title
Demo (XMarkdown)

| banner
[Scripta.io](https://scripta.io)

| contents

# Images

![Yellow bird](https://i.ibb.co/XFzZYby/image.png)

# Math

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

*bold Tip:* Click on a section title to go back to the table of contents.

"""

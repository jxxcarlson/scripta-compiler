module Text exposing (microLaTeXDemo, l0Demo, xMarkdown, info)


info = """
| title
About the Scripta compiler

[tags jxxcarlson:about-the-scripta-compiler]

| runninghead
[link Scripta.io https://scripta.io]

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

The Scripta compiler is open-source, and can be found at 
[link github.com/jxxcarlson/scripta-compiler  https://github.com/jxxcarlson/scripta-compiler].  In the Example
folder, you will find a small demo app.  It is hosted online
at [link Github https://jxxcarlson.github.io/app/scripta-compiler-demo/assets/index.html].

The Scripta compiler is used to power 
[link Scripta.io https://scripta.io].  It features
interactive editing, a searchable store of documents, 
and facilities for collaboration and web publishing.
"""

microLaTeXDemo = """
\\title{Demo (MicroLaTeX)}

| banner
\\link{Scrpta.io https://scripta.io}

\\contents

\\section{Images}

\\image{https://see.news/wp-content/uploads/2020/12/UK_wildbirds-01-robin.jpg}

\\section{Math}

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

"""


l0Demo = """
| title
Demo (L0)

| banner
[link Scripta.io https://scripta.io]

| contents

| section 1
Images

[image https://nas-national-prod.s3.amazonaws.com/styles/hero_image/s3/web_h_apa_2016-a1_2474_8_cedar-waxwing_peter_brannon_kk_female.jpg?itok=VdeVVmGA]

| section 1
Math

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

"""

xMarkdown = """
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

"""

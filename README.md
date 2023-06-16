# Scripta Compiler

The Scripta compiler transforms markup source text 
(L0, microLaTeX, XMarkdown) to HTML. It is 
written in [Elm](https://elm-lang.org) and 
is used in the apps described below.


The Scripta project (compiler and apps) is pre-release software, but in pretty good shape.
I've used it extensively, mostly for courses  and lecture notes. I welcome comments and suggestions.

This project was partially supported by a grant from the Simons Foundation.  
I wish to thank the Foundation for their generosity.

NOTE: I am now working on version 2 of the compiler.  It features a more modular design and a much simplifie API.

## Contact

Jim Carlson: jxxcarlson at gmail, the Elm Slack,
and this GitHub repository to post issues.

## Demos and apps



- Code:  `src/Example1` and `src/Example2`

- Live demo: 
[Example2@Github](https://jxxcarlson.github.io/app/scripta-compiler-demo/index.html)

- [Scripta Desktop](https://github.com/jxxcarlson/scripta-tauri/releases)
  is a desktop version of Scripta.io.  It has less functionality,
  but can be used without an internet condition.  Files are stored
  in the user's computer.  Scripta Desktop uses [Tauri](https://tauri.app)
  to turn Elm code into a desktop.  The build process is described
  [here](https://jxxcarlson.medium.com/elm-tauri-befa59eab403).

 
## Scripta.io

The [Scripta web app](https://scripta.io) at Script.io
provides an editing and publication platform
for markup source text in
L0, MicroLaTeX, aand XMarkdown. The app is written
in [Elm](https://elm-lang.org) using the
[Lamdera](https://lamdera.com) framework so that
both the front and back ends run on elm.  There
are two large Javascript components:
[Codemirror 6](https://codemirror.net/) for the
editor and [KaTeX](https://katex.org) for rendering
mathematical formulas.  There is a small (~200 lines)
server app written in Haskell that is used to
convert LaTeX output generated by Scripta.io
into PDF.

Scripta.io works on computers and tablets.  On small
tablets and smart phones, it transforms itself
into Scripta Reader.  In this form the user can read
any public Scripta.io document without having to 
sign in or to have an account.

Scripta.io is still pre-release, but in pretty
good shape. I've used it extensively, mostly
for courses and lecture notes. I welcome
comments and suggestions.







## Documentation

- [Compiler Documentation](https://jxxcarlson.github.io/docs-scripta-compiler)

- [Scripta User Documentation](https://jxxcarlson.github.io/docs-scripta)

- [Scripta App Documentation](https://jxxcarlson.github.io/docs-scripta-dev)




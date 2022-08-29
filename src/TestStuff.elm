module TestStuff exposing (..)

import Compiler.DifferEq
import Markup
import Parser.PrimitiveBlock
import Scripta.Language exposing (Language(..))


toPrimitiveBlocks =
    Markup.toPrimitiveBlocks L0Lang


diffc a b =
    Compiler.DifferEq.diffc Parser.PrimitiveBlock.eq .indent (toPrimitiveBlocks a) (toPrimitiveBlocks b)


textA1 =
    """abc

  def

  ghi

jkl
"""


textB1 =
    """abc

  def!

  ghi

jkl
"""


textB =
    String.replace "In this case, $G$ is the functor with" "In this case, $G$ is the functor wieth" textA


textA =
    """
| title
Category Theory Macros

[tags jxxcarlson:category-theory-macros]


|| mathmacros
\\newcommand{\\op}[1]{\\mathop{\\text{#1}}}
\\newcommand{\\Set}{\\mathop{\\underline{\\text{Set}}}}
\\newcommand{\\Group}{\\mathop{\\underline{\\text{Group}}}}
\\newcommand{\\Etale}{\\mathop{\\bold{Etale}}}
\\newcommand{\\Sh}{\\op{Sh}}
\\newcommand{\\bN}{\\mathbb{N}}
\\newcommand{\\cA}{\\mathcal{A}}
\\newcommand{\\cB}{\\mathcal{B}}
\\newcommand{\\cC}{\\mathcal{C}}
\\newcommand{\\cD}{\\mathcal{D}}
\\newcommand{\\cO}{\\mathcal{O}}
\\newcommand{\\cH}{\\mathcal{H}}
\\newcommand{\\cP}{\\mathcal{P}}
\\newcommand{\\cV}{\\mathcal{V}}
\\newcommand{\\Hom}{\\op{Hom}}
\\newcommand{\\Prop}{\\op{Prop}}
\\newcommand{\\cat}[1]{\\mathcal{#1}}
\\newcommand{\\nat}{\\mathbb{N}}
\\newcommand{\\reals}{\\mathbb{R}}
\\newcommand{\\set}[1]{\\{ #1 \\}}
\\newcommand{\\sett}[2]{\\{ #1\\ |\\ #2 \\}}
\\newcommand {\\hom}{\\mathop{\\text{hom}}}


"""

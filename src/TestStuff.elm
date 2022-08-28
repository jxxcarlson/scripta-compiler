module TestStuff exposing (textA, textB, textA1, textB1)


textA1 = """
abc

def

ghi

jkl
"""

textB1 = """
abc

def!

ghi

jkl
"""

textA = """
|| load-files
jxxcarlson:category-theory-macros

| banner
[ilink Category Theory Notesjxxcarlson:folder-category-theory]


| title
Graphs and Colorings ORIG 

| contents

| section 1
Graphs as Functors


See this diagram (Fig [ref abcd-y-graph])

Morphism: Figure [ref graph-morphism ]

Natural transformation: Figure [ref morphism-graph]

Complete Graph: (Figure [ref complete-graph])

A bird: [eqref bird2]

[tags jxxcarlson:graphs-and-colorings, folder:category-theory-notes]

Let $\\Gamma$ be the category with objects $E$ and $V$
and with arrows $s, t: E \\to V$, where $s$ picks out
the source (tail) of an arrow and $t$ picks out the 
target (head):

|| equation
\\Gamma = (E \\xrightarrow{s, t} V)

A [term graph] is a functor $G: \\Gamma \\to \\Set$.  Consider, 
for example, the graph in Figure [ref abcd-y-graph] beluow:


|| quiver width:300 label:abcd-y-graph
https://i.ibb.co/0j2Dt8B/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwxLCJhIl0sWzIsMSwiYiJdLFs0LDAsImMiXSxbNCwyLCJkIl0sWzAsMV0sWzEsMl0sWzEsM11d
\\[\\begin{tikzcd}
	&&&& c \\
	a && b \\
	&&&& d
	\\arrow[from=2-1, to=2-3]
	\\arrow[from=2-3, to=1-5]
	\\arrow[from=2-3, to=3-5]
\\end{tikzcd}\\]

In this case, $G$ is the functor with 

|| aligned
G(V) &= \\set{a, b, c, d} \\
G(E) &= \\set{ab, bc, bd}

where $ab$ is the edge connecting $a$ to $b$, etc.  
Since $G$ is a functor, we have

|| aligned
G(s)&: G(E) \\to G(V) \\
G(t)&: G(E) \\to G(V) 

Thus $G(s)(ab) = a$, $G(t)(ab) = b$, etc.

Let $\\op{Graph} = [\\Gamma, \\Set]$ be the (functor) category of directed graphs.  Then a morphism of graphs is a natural
transformation of functors $\\eta$.  Thus, if $G$ and $G'$ are graphs, and $\\eta : G \\to G'$ is a morphism, then 
$\\eta_{E} : G(E) \\to G'(E)$ maps edges of the first graph
to edges of the second and $\\eta_V : G(V) \\to G'(V)$
maps vertices to vertices.   

|| quiver width:300 caption: Morphism of graphs label:morphism-graph
https://i.ibb.co/NLtvMbg/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwwLCJHKEUpIl0sWzIsMCwiRycoRSkiXSxbMiwyLCJHJyhWKSJdLFswLDIsIkcoVikiXSxbMCwzLCJHKHMpIiwyXSxbMSwyLCJHJyhzKSJdLFswLDEsIlxcZXRhX0UiXSxbMywyLCJcXGV0YV9WIiwyXV0=
\\[\\begin{tikzcd}
	{G(E)} && {G'(E)} \\
	\\
	{G(V)} && {G'(V)}
	\\arrow["{G(s)}"', from=1-1, to=3-1]
	\\arrow["{G'(s)}", from=1-3, to=3-3]
	\\arrow["{\\eta_E}", from=1-1, to=1-3]
	\\arrow["{\\eta_V}"', from=3-1, to=3-3]
\\end{tikzcd}\\]

Test: [eqref functoriality]

The functoriality equations coming out of Figure 
[ref morphism-graph],


|| aligned
[label functoriality]
\\eta_V \\circ G(s) = G'(s) \\circ \\eta_{E} \\
\\eta_V \\circ G(t) = G'(t) \\circ \\eta_{E} \\

assert that these assignments of edges and vertices
preserve incidence relations. As an example, consider the
morphism pictured in the Figure [ref graph-morphism] below:

|| quiver width:300 label:graph-morphism caption:Morphism of graphs 
https://i.ibb.co/hWnDsym/image.png
---
% https://q.uiver.app/?q=WzAsNyxbMCwxLCJhIl0sWzIsMSwiYiJdLFswLDQsIjEiXSxbMiw0LCIyIl0sWzQsNCwiMyJdLFs0LDAsImMiXSxbNCwyLCJkIl0sWzAsMV0sWzIsM10sWzMsNF0sWzAsMiwiXFxldGFfYSIsMSx7InN0eWxlIjp7ImJvZHkiOnsibmFtZSI6ImRhc2hlZCJ9fX1dLFsxLDMsIlxcZXRhX2IiLDEseyJzdHlsZSI6eyJib2R5Ijp7Im5hbWUiOiJkYXNoZWQifX19XSxbMSw1XSxbNSw0LCJcXGV0YV9jIiwxLHsiY3VydmUiOi01LCJzdHlsZSI6eyJib2R5Ijp7Im5hbWUiOiJkYXNoZWQifX19XSxbMSw2XSxbNiw0LCJcXGV0YV9kIiwxLHsic3R5bGUiOnsiYm9keSI6eyJuYW1lIjoiZGFzaGVkIn19fV1d
\\[\\begin{tikzcd}
	&&&& c \\
	a && b \\
	&&&& d \\
	\\
	1 && 2 && 3
	\\arrow[from=2-1, to=2-3]
	\\arrow[from=5-1, to=5-3]
	\\arrow[from=5-3, to=5-5]
	\\arrow["{\\eta_a}"{description}, dashed, from=2-1, to=5-1]
	\\arrow["{\\eta_b}"{description}, dashed, from=2-3, to=5-3]
	\\arrow[from=2-3, to=1-5]
	\\arrow["{\\eta_c}"{description}, curve={height=-30pt}, dashed, from=1-5, to=5-5]
	\\arrow[from=2-3, to=3-5]
	\\arrow["{\\eta_d}"{description}, dashed, from=3-5, to=5-5]
\\end{tikzcd}\\]


| section 1
Colorings



A [term coloring] of a graph is a function $c$ from its vertices to a set $C$ such that vertices 
connected by an edge have different colors:

|| image label:bird2
https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR5bSkw7pGVBDgAQTm4VMgoE_NmPBn29rNXSw&usqp=CAU

|| equation
\\forall e \\in G(E): c(G(s)(e)) \\ne c(G(t)(e))

If $C$ has $n$ elements, then $c$ is an $n$-coloring. As an example, the graph in Figure 1 as a 2-coloring
with red and green, as indicated in Figure [ref fig-colored-graph]

|| quiver width:300 label:fig-colored-graph Colored graph
https://i.ibb.co/r5ZskWg/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwxLCJhLCBSIl0sWzIsMSwiYiwgRyJdLFs0LDAsImMsIFIiXSxbNCwyLCJkLCBSIl0sWzAsMV0sWzEsMl0sWzEsM11d
\\[\\begin{tikzcd}
	&&&& {c, R} \\
	{a, R} && {b, G} \\
	&&&& {d, R}
	\\arrow[from=2-1, to=2-3]
	\\arrow[from=2-3, to=1-5]
	\\arrow[from=2-3, to=3-5]
\\end{tikzcd}\\]

Let $C_n : \\op{Graph} \\to \\Set$ be the functor which assigns to a graph the corresponding set of $n$-colorings.
(To make this statement precise, we assume that colors are 
taken from a fixed but infinite set of color names.  The 
set $\\bN$ of natural numbers will do.) We say that a graph
$G$ is [term n-colorable] if $C_n(G)$ is non-empty.  A
famous theorem asserts that every planar graph is 4-colorable.

| problem
What happens to the colorability of the graph in Figure [ref fig-colored-graph] if
we add the vertex $cd$?

| section 1
Complete graphs

A complete graph is one in which any two vertices are 
connected by an edge, as in Figure [ref complete-graph].

|| quiver width:250 label:complete-graph Complete Graph
https://i.ibb.co/MNF2dqJ/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwwLCJBIl0sWzIsMCwiRCJdLFswLDIsIkIiXSxbMiwyLCJDIl0sWzAsMl0sWzIsM10sWzMsMV0sWzEsMF0sWzAsM10sWzEsMl1d
\\[\\begin{tikzcd}
	A && D \\
	\\
	B && C
	\\arrow[from=1-1, to=3-1]
	\\arrow[from=3-1, to=3-3]
	\\arrow[from=3-3, to=1-3]
	\\arrow[from=1-3, to=1-1]
	\\arrow[from=1-1, to=3-3]
	\\arrow[from=1-3, to=3-1]
\\end{tikzcd}\\]


Let $K_n$ be the graph with vertices $\\set{1, \\ldots, n}$
and with edges $ij$, for all $i < j$.  There is a natural 
bijection of sets

|| equation
C_n(G) \\to \\Hom(G, K_n)

"""



textB = """
|| load-files
jxxcarlson:category-theory-macros

| banner
[ilink Category Theory Notesjxxcarlson:folder-category-theory]


| title
Graphs and Colorings ORIG

| contents

| section 1
Graphs as Functors


See this diagram (Fig [ref abcd-y-graph])

Morphism: Figure [ref graph-morphism ]

Natural transformation: Figure [ref morphism-graph]

Complete Graph: (Figure [ref complete-graph])

A bird: [eqref bird2]

[tags jxxcarlson:graphs-and-colorings, folder:category-theory-notes]

Let $\\Gamma$ be the category with objects $E$ and $V$
and with arrows $s, t: E \\to V$, where $s$ picks out
the source (tail) of an arrow and $t$ picks out the
target (head):

|| equation
\\Gamma = (E \\xrightarrow{s, t} V)

A [term graph] is a functor $G: \\Gamma \\to \\Set$.  Consider,
for example, the graph in Figure [ref abcd-y-graph] beluow:


|| quiver width:300 label:abcd-y-graph
https://i.ibb.co/0j2Dt8B/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwxLCJhIl0sWzIsMSwiYiJdLFs0LDAsImMiXSxbNCwyLCJkIl0sWzAsMV0sWzEsMl0sWzEsM11d
\\[\\begin{tikzcd}
	&&&& c \\
	a && b \\
	&&&& d
	\\arrow[from=2-1, to=2-3]
	\\arrow[from=2-3, to=1-5]
	\\arrow[from=2-3, to=3-5]
\\end{tikzcd}\\]

In this case, $G$ is the functor wieth

|| aligned
G(V) &= \\set{a, b, c, d} \\
G(E) &= \\set{ab, bc, bd}

where $ab$ is the edge connecting $a$ to $b$, etc.
Since $G$ is a functor, we have

|| aligned
G(s)&: G(E) \\to G(V) \\
G(t)&: G(E) \\to G(V)

Thus $G(s)(ab) = a$, $G(t)(ab) = b$, etc.

Let $\\op{Graph} = [\\Gamma, \\Set]$ be the (functor) category of directed graphs.  Then a morphism of graphs is a natural
transformation of functors $\\eta$.  Thus, if $G$ and $G'$ are graphs, and $\\eta : G \\to G'$ is a morphism, then
$\\eta_{E} : G(E) \\to G'(E)$ maps edges of the first graph
to edges of the second and $\\eta_V : G(V) \\to G'(V)$
maps vertices to vertices.

|| quiver width:300 caption: Morphism of graphs label:morphism-graph
https://i.ibb.co/NLtvMbg/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwwLCJHKEUpIl0sWzIsMCwiRycoRSkiXSxbMiwyLCJHJyhWKSJdLFswLDIsIkcoVikiXSxbMCwzLCJHKHMpIiwyXSxbMSwyLCJHJyhzKSJdLFswLDEsIlxcZXRhX0UiXSxbMywyLCJcXGV0YV9WIiwyXV0=
\\[\\begin{tikzcd}
	{G(E)} && {G'(E)} \\
	\\
	{G(V)} && {G'(V)}
	\\arrow["{G(s)}"', from=1-1, to=3-1]
	\\arrow["{G'(s)}", from=1-3, to=3-3]
	\\arrow["{\\eta_E}", from=1-1, to=1-3]
	\\arrow["{\\eta_V}"', from=3-1, to=3-3]
\\end{tikzcd}\\]

Test: [eqref functoriality]

The functoriality equations coming out of Figure
[ref morphism-graph],


|| aligned
[label functoriality]
\\eta_V \\circ G(s) = G'(s) \\circ \\eta_{E} \\
\\eta_V \\circ G(t) = G'(t) \\circ \\eta_{E} \\

assert that these assignments of edges and vertices
preserve incidence relations. As an example, consider the
morphism pictured in the Figure [ref graph-morphism] below:

|| quiver width:300 label:graph-morphism caption:Morphism of graphs
https://i.ibb.co/hWnDsym/image.png
---
% https://q.uiver.app/?q=WzAsNyxbMCwxLCJhIl0sWzIsMSwiYiJdLFswLDQsIjEiXSxbMiw0LCIyIl0sWzQsNCwiMyJdLFs0LDAsImMiXSxbNCwyLCJkIl0sWzAsMV0sWzIsM10sWzMsNF0sWzAsMiwiXFxldGFfYSIsMSx7InN0eWxlIjp7ImJvZHkiOnsibmFtZSI6ImRhc2hlZCJ9fX1dLFsxLDMsIlxcZXRhX2IiLDEseyJzdHlsZSI6eyJib2R5Ijp7Im5hbWUiOiJkYXNoZWQifX19XSxbMSw1XSxbNSw0LCJcXGV0YV9jIiwxLHsiY3VydmUiOi01LCJzdHlsZSI6eyJib2R5Ijp7Im5hbWUiOiJkYXNoZWQifX19XSxbMSw2XSxbNiw0LCJcXGV0YV9kIiwxLHsic3R5bGUiOnsiYm9keSI6eyJuYW1lIjoiZGFzaGVkIn19fV1d
\\[\\begin{tikzcd}
	&&&& c \\
	a && b \\
	&&&& d \\
	\\
	1 && 2 && 3
	\\arrow[from=2-1, to=2-3]
	\\arrow[from=5-1, to=5-3]
	\\arrow[from=5-3, to=5-5]
	\\arrow["{\\eta_a}"{description}, dashed, from=2-1, to=5-1]
	\\arrow["{\\eta_b}"{description}, dashed, from=2-3, to=5-3]
	\\arrow[from=2-3, to=1-5]
	\\arrow["{\\eta_c}"{description}, curve={height=-30pt}, dashed, from=1-5, to=5-5]
	\\arrow[from=2-3, to=3-5]
	\\arrow["{\\eta_d}"{description}, dashed, from=3-5, to=5-5]
\\end{tikzcd}\\]


| section 1
Colorings



A [term coloring] of a graph is a function $c$ from its vertices to a set $C$ such that vertices
connected by an edge have different colors:

|| image label:bird2
https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR5bSkw7pGVBDgAQTm4VMgoE_NmPBn29rNXSw&usqp=CAU

|| equation
\\forall e \\in G(E): c(G(s)(e)) \\ne c(G(t)(e))

If $C$ has $n$ elements, then $c$ is an $n$-coloring. As an example, the graph in Figure 1 as a 2-coloring
with red and green, as indicated in Figure [ref fig-colored-graph]

|| quiver width:300 label:fig-colored-graph Colored graph
https://i.ibb.co/r5ZskWg/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwxLCJhLCBSIl0sWzIsMSwiYiwgRyJdLFs0LDAsImMsIFIiXSxbNCwyLCJkLCBSIl0sWzAsMV0sWzEsMl0sWzEsM11d
\\[\\begin{tikzcd}
	&&&& {c, R} \\
	{a, R} && {b, G} \\
	&&&& {d, R}
	\\arrow[from=2-1, to=2-3]
	\\arrow[from=2-3, to=1-5]
	\\arrow[from=2-3, to=3-5]
\\end{tikzcd}\\]

Let $C_n : \\op{Graph} \\to \\Set$ be the functor which assigns to a graph the corresponding set of $n$-colorings.
(To make this statement precise, we assume that colors are
taken from a fixed but infinite set of color names.  The
set $\\bN$ of natural numbers will do.) We say that a graph
$G$ is [term n-colorable] if $C_n(G)$ is non-empty.  A
famous theorem asserts that every planar graph is 4-colorable.

| problem
What happens to the colorability of the graph in Figure [ref fig-colored-graph] if
we add the vertex $cd$?

| section 1
Complete graphs

A complete graph is one in which any two vertices are
connected by an edge, as in Figure [ref complete-graph].

|| quiver width:250 label:complete-graph Complete Graph
https://i.ibb.co/MNF2dqJ/image.png
---
% https://q.uiver.app/?q=WzAsNCxbMCwwLCJBIl0sWzIsMCwiRCJdLFswLDIsIkIiXSxbMiwyLCJDIl0sWzAsMl0sWzIsM10sWzMsMV0sWzEsMF0sWzAsM10sWzEsMl1d
\\[\\begin{tikzcd}
	A && D \\
	\\
	B && C
	\\arrow[from=1-1, to=3-1]
	\\arrow[from=3-1, to=3-3]
	\\arrow[from=3-3, to=1-3]
	\\arrow[from=1-3, to=1-1]
	\\arrow[from=1-1, to=3-3]
	\\arrow[from=1-3, to=3-1]
\\end{tikzcd}\\]


Let $K_n$ be the graph with vertices $\\set{1, \\ldots, n}$
and with edges $ij$, for all $i < j$.  There is a natural
bijection of sets

|| equation
C_n(G) \\to \\Hom(G, K_n)

"""
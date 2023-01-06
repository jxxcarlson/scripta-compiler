# Overview


## Flowchart

The Scripta compiler transforms text into `Element Msg`
through a series of stages, as illustrated in the
following figure. Below we describe these stages.


![Flowchart](image/scripta-compiler-flowchart.jpg)

The first step is to break the source text into chunks,
namely a list of so-called _primitive blocks_. Ignoring
for the moment the possibility of diff records and
differential compilation, we traverse the left branch
of the diagram, the penultimate step of which is the
production of an `EditRecord`.  Data of this type
is transformed into an Elm representation of Html
by the function 
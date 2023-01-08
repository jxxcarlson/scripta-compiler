# Synchronization


## Sourcemap

The `id` field of an `ExpressionBlock` is simply
the string version of the line number field of the `PrimitiveBlock`
from which it is derived.  The `id` field is used in rendered-to-source
syncrhonization.  Namely, when the user clicks on a piece of rendered
text, the message `SendId id` is sent.  When this is handled, the
corresponding source text is scrolled into view and highlighted.
(NOTE: still quite innacurate).  In both Scripta.io and Scripta Desktop,
the message is passed on to the Codemirror editor, whihc does the
scrolling and highlighting.

PROBLEM: when source text is edited and differential parsing is used,
line numbers after the edited text are not update.

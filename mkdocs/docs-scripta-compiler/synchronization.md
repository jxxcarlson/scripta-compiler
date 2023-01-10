# Synchronization

## Synchronizing Rendered Text to Source Text

The `lineNumber` field of an `ExpressionBlock` is used for 
rendered-text-to-source syncrhonization.  Namely, when the user 
clicks on a piece of rendered
text, the message `SendLineNumber lineNumber` is sent using

```text
sendLineNumberOnClick : Int -> Element.Attribute MarkupMsg
sendLineNumberOnClick lineNumber =
    Events.onClick (SendLineNumber (String.fromInt lineNumber))
```


When the message is handled by the host app and editor, the
corresponding source text is scrolled into view and highlighted.
(NOTE: still quite innacurate).  In both Scripta.io and Scripta Desktop,
the message is passed on to the Codemirror editor, whihc does the
scrolling and highlighting.

PROBLEM: there are still some problems stemming from innaccurate
computation of line numbers by the differential parser.  To 
be resolved.


## Synchronizing Source Text to Rendered Text

Although synchronization is carried out by the host
app, we outine the process here.  First, the user
highlights a block of source text, then pressed a button
or key combination to send a message from the editor to
the Elm app.  On the JS side, the editor code 
for this in Codemirror 6 is

```text
attributeChangedCallback(attr, oldVal, newVal) {

         function sendSelectedText(editor, str) {
                     console.log("sendSelectedText (dispatch)", str)
                     const event = new CustomEvent('selected-text', { 'detail': str , 'bubbles':true, 'composed': true});
                     editor.dom.dispatchEvent(event);
                  }
                  
        ...
```

The selected text is handle by

```text
onSelectionChange : Html.Attribute FrontendMsg
onSelectionChange =
    textDecoder
        |> Json.Decode.map SelectedText
        |> Html.Events.on "selected-text"
```

which passes it to the app's (front end) update function 
in clause `SelectedText`:
 
```text
SelectedText selectedText ->
  Frontend.Update.firstSyncLR model selectedText
```

The `firstSyncLR` function searches the syntax tree (forest)
for the given text using


```text
ompiler.ASTTools.matchingIdsInAST selectedText model.editRecord.tree
```

This is possible because one field of the corresponding 
`ExpressionBlock` is the source text from which it was
derived.  Once the id(s) for corresponding elements
of the rendered text are found, they can be scrolled
into view and highlighted.
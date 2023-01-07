# Primitive Blocks

Blocks have the rather elaborate type listed below.  
This complex structure is necessary for interactive
real-time editing with error recovery.  By real-time we mean
that the rendered text is updated "instantaneously"
in the browser as the user types in text.  By interactive,
we mean (for example) that click on the rendered text
brings the associated source text into view while
simultaneously highlighting it.  In addition, if the
user selects a block of source text and presses
ctrl-S (S for 'sync'), the corresponding rendered text
is brought into view and highlighted.




```
type ExpressionBlock
    = ExpressionBlock
        { name : Maybe String
        , args : List String
        , properties : Dict String String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , id : String
        , tag : String
        , blockType : BlockType
        , content : Either String (List Expr)
        , messages : List String
        , sourceText : String
        , error : Maybe { error : String }
        }
```


## Edit records


```
type alias EditRecord chunk parsedChunk accumulator =
    { chunks : List chunk
    , parsed : List parsedChunk
    , tree : List (Tree parsedChunk)
    , accumulator : accumulator
    , lang : Language
    , messages : List String
    , initialData : InitialData
    }
```


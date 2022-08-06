module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import CognitiveComplexity
import NoDebug.Log
import NoExposingEverything
import NoImportingEverything
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoImportingEverything.rule [ "Element" ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoExposingEverything.rule
    , NoDebug.Log.rule

    --, CognitiveComplexity.rule 25
    ]
        |> List.map
            (Review.Rule.ignoreErrorsForFiles
                [ "src/Env.elm" -- reports "Production" as unused constructor. This is used by Lamdera in deploy.
                , "src/Types.elm" -- 23/26 false positives
                , "src/CollaborativeEditing/NetworkSimulator2.elm" -- experimental, not used in app
                , "src/CollaborativeEditing/NetworkSimulator3.elm" -- experimental, not used in app
                , "vendor/jinjor/elm-debounce/3.0.0/src/Debounce.elm" -- vendored: not touching
                , "compiler/Tools.elm" -- Deliberately excluded: used only in debugging
                , "compiler/Render/Data.elm" -- work in progress
                , "compiler/Parser/Tools.elm" -- Evan's Parser.Advanced requires a nonempty Context type even though I am not using that part of Parser.Advanced.
                , "compiler/Compiler/Acc.elm" -- I don't want to remove 'lang' in 'transformAccumulate lang ast' until I am sure it is not going to be needed (work in progress)
                ]
            )
        |> List.map (Review.Rule.ignoreErrorsForDirectories [ "src/Evergreen" ])

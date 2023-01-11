module Utility exposing (compressWhitespace, removeNonAlphaNum, userReplace)

import Regex


compressWhitespace : String -> String
compressWhitespace string =
    userReplace "\\s\\s+" (\_ -> " ") string


removeNonAlphaNum : String -> String
removeNonAlphaNum string =
    userReplace "[^A-Za-z0-9\\-]" (\_ -> "") string


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string

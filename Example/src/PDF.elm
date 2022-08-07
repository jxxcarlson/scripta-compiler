module PDF exposing (gotLink, printCmd, pdfServUrl, PDFMsg(..), PrintingState(..))

import Compiler.ASTTools as ASTTools
import Process
import Task
import Time
import Either
import Json.Encode as E
import Maybe.Extra
import Parser.Block exposing (ExpressionBlock(..))
import Render.Export.LaTeX
import Render.Settings exposing (Settings)
import Tree
import Http
import Parser.Forest exposing(Forest)
import Scripta.API

pdfServUrl1  = "https://pdfserv.app/pdf/"

pdfServUrl = "http://localhost:3000/pdf/"

type PDFMsg = ChangePrintingState PrintingState | GotPdfLink (Result Http.Error String)

type PrintingState
    = PrintWaiting
    | PrintProcessing
    | PrintReady

printCmd :  Time.Posix -> Settings -> Forest ExpressionBlock -> Cmd PDFMsg
printCmd currentTime settings forest =
     Cmd.batch
        [
         Process.sleep 30 |> Task.perform (always (ChangePrintingState PrintProcessing))
        , generatePdf currentTime settings forest
        ]

generatePdf : Time.Posix -> Settings -> Forest ExpressionBlock -> Cmd PDFMsg
generatePdf currentTime settings syntaxTree =
    let

        imageUrls : List String
        imageUrls =
            syntaxTree
                |> List.map Tree.flatten
                |> List.concat
                |> List.map (\(ExpressionBlock { content }) -> Either.toList content)
                |> List.concat
                |> List.concat
                |> ASTTools.filterExpressionsOnName "image"
                |> List.map (ASTTools.getText >> Maybe.map String.trim)
                |> List.map (Maybe.andThen extractUrl)
                |> Maybe.Extra.values

        contentForExport =
            Render.Export.LaTeX.export currentTime settings syntaxTree

        fileName = Scripta.API.fileNameForExport syntaxTree

    in
    Cmd.batch
        [ Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = pdfServUrl
            , body = Http.jsonBody (encodeForPDF fileName contentForExport imageUrls)
            , expect = Http.expectString GotPdfLink
            , timeout = Nothing
            , tracker = Nothing
            }
        ]


extractUrl : String -> Maybe String
extractUrl str =
    str |> String.split " " |> List.head


gotLink : model -> Result error value -> ( model, Cmd PDFMsg )
gotLink model result =
    case result of
        Err _ ->
            ( model, Cmd.none )

        Ok _ ->
            ( model
            , Cmd.batch
                [ Process.sleep 5 |> Task.perform (always (ChangePrintingState PrintReady))
                ]
            )


encodeForPDF : String -> String -> List String -> E.Value
encodeForPDF id content urlList =
    E.object
        [ ( "id", E.string id )
        , ( "content", E.string content )
        , ( "urlList", E.list E.string urlList )
        ]

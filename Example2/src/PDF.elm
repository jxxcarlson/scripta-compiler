module PDF exposing
    ( PDFMsg(..)
    , PrintingState(..)
    , TarFileState(..)
    , gotLink
    , pdfServUrl
    , printCmd
    , tarArchiveUrl
    , tarCmd
    )

import Http
import Json.Encode as E
import Process
import Scripta.API
import Task
import Time


pdfServUrl1 =
    "https://pdfserv.app/pdf/"



--
--pdfServUrl = "http://localhost:3000/pdf/"
--tarArchiveUrl = "http://localhost:3000/tar/"


pdfServUrl =
    "https://pdfServ.app/pdf/"


tarArchiveUrl =
    "https://pdfServ.app/tar/"


type PDFMsg
    = ChangePrintingState PrintingState
    | ChangeTarFileState TarFileState
    | GotPdfLink (Result Http.Error String)
    | GotTarFile (Result Http.Error String)


type PrintingState
    = PrintWaiting
    | PrintProcessing
    | PrintReady


type TarFileState
    = TarFileWaiting
    | TarFileProcessing
    | TarFileReady


printCmd : Time.Posix -> Scripta.API.Settings -> Scripta.API.EditRecord -> Cmd PDFMsg
printCmd currentTime settings forest =
    Cmd.batch
        [ Process.sleep 30 |> Task.perform (always (ChangePrintingState PrintProcessing))
        , pdfCmd currentTime settings forest
        ]


pdfCmd1 : Time.Posix -> Scripta.API.Settings -> Scripta.API.EditRecord -> Cmd PDFMsg
pdfCmd1 currentTime settings editRecord =
    let
        imageUrls : List String
        imageUrls =
            Scripta.API.getImageUrls editRecord.tree

        packageNames = Scripta.API.packageNames editRecord.tree

        fileName =
            Scripta.API.fileNameForExport editRecord.tree

        contentForExport =
            Scripta.API.prepareContentForExport currentTime settings editRecord.tree
    in
    Cmd.batch
        [ Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = pdfServUrl
            , body = Http.jsonBody (encodeForPDF fileName contentForExport imageUrls packageNames)
            , expect = Http.expectString GotPdfLink
            , timeout = Nothing
            , tracker = Nothing
            }
        ]


pdfCmd : Time.Posix -> Scripta.API.Settings -> Scripta.API.EditRecord -> Cmd PDFMsg
pdfCmd currentTime settings editRecord =
    Cmd.batch
        [ Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = pdfServUrl
            , body = Http.jsonBody (Scripta.API.encodeForPDF currentTime settings editRecord.tree)
            , expect = Http.expectString GotPdfLink
            , timeout = Nothing
            , tracker = Nothing
            }
        ]


tarCmd : Time.Posix -> Scripta.API.Settings -> Scripta.API.EditRecord -> Cmd PDFMsg
tarCmd currentTime settings editRecord =
    let
            imageUrls : List String
            imageUrls =
                Scripta.API.getImageUrls editRecord.tree

            packageNames = Scripta.API.packageNames editRecord.tree

            fileName =
                Scripta.API.fileNameForExport editRecord.tree

            contentForExport =
                Scripta.API.prepareContentForExport currentTime settings editRecord.tree
        in
    Cmd.batch
        [ Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = tarArchiveUrl
            , body = Http.jsonBody (encodeForPDF fileName contentForExport imageUrls packageNames)
            , expect = Http.expectString GotTarFile
            , timeout = Nothing
            , tracker = Nothing
            }
        ]


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


encodeForPDF : String -> String -> List String -> List String -> E.Value
encodeForPDF id content urlList packageNames =
    E.object
        [ ( "id", E.string id )
        , ( "content", E.string content )
        , ( "urlList", E.list E.string urlList )
        , ( "packageList",  E.list E.string [] )
        ]

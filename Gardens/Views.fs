module Gardens.Views

open Giraffe.ViewEngine

let layout (content: XmlNode list) =
    html [] [
        head [] [
            title []  [ encodedText "Gardens" ]
            link [
                _rel  "stylesheet"
                _type "text/css"
                _href "/main.css"
            ]
        ]
        body [] content
    ]

let partial () =
    h1 [] [ encodedText "Gardens" ]

let index (name : string) =
    [
        partial()
        p [] [ encodedText name ]
    ]
    |> layout
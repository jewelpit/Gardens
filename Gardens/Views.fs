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

let index (garden : Model.Garden) =
    layout [
        h1 [] [str "ASCII Garden"]
        p [] [str (sprintf "%d ticks old" garden.Ticks)]
        p [] [
            pre [] [str (garden.ToString())]
        ]
    ]
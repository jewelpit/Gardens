module Gardens.Views

open FSharp.Control.Tasks.Affine
open Giraffe.ViewEngine

let layout (content: XmlNode list) =
    html [] [
        head [] [
            title []  [ encodedText "Gardens" ]
            link [
                _rel "stylesheet"
                _type "text/css"
                _href "/static/main.css"
            ]
            script [
                _src "/static/client.js"
            ] []
        ]
        body [] content
    ]

let index (garden : Model.Garden) =
    task {
        let! state = garden.GetState()
        return layout [
            h1 [] [str "ASCII Garden"]
            p [_id "age"] [str (sprintf "Age: %d ticks" state.Tick)]
            p [] [
                pre [_id "garden"] [str (state.Garden.Value)]
            ]
        ]
    }
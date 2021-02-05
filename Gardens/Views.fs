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
        let! state = garden.GetState(None)
        return layout [
            div [_class "flex"] [
                div [_class "spacer"] []
                div [] [
                    h1 [] [str "ASCII Garden"]
                    div [_class "flex"] [
                        div [_id "age"; _class "spacer left"] [str (sprintf "Age: %d ticks" state.Tick)]
                        div [_id "numPlants"; _class "spacer center"] [str (sprintf "Plants: %d" state.NumPlants)]
                        div [_id "numWatchers"; _class "spacer right"] [str (sprintf "Watchers: %d" (state.NumWatchers + 1))]
                    ]
                    pre [_id "garden"] [str (state.Garden.Value)]
                ]
                div [_class "spacer"] []
            ]
        ]
    }
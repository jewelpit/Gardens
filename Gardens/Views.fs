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

let index (gardenManager : Model.GardenManager) =
    layout [
        form [_action "/add_garden"; _method "post"] [
            input [_type "submit"; _value "Create a new garden!"]
        ]
        yield! gardenManager.Gardens
            |> Seq.map (fun kvp -> p [] [a [_href (sprintf "/garden/%d" kvp.Key)] [str kvp.Value.Name]])
    ]

let garden (garden : Model.Garden) =
    layout [
        h1 [] [str garden.Name]
    ]
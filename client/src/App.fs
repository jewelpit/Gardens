module Gardens.Client.App

open System
open System.Text

open Browser
open Fable.Core
open Fable.Core.JS
open Fable.Remoting.Client

type State =
    | Active of Types.Update
    | Disconnected

let watcherId = Guid.NewGuid().ToString()
printfn "%A" watcherId

let remoteApi =
    Remoting.createApi()
    |> Remoting.buildProxy<Types.RemoteApi>

let mutable lastTick: int64 = 0L
let ageDiv = document.getElementById("age")
let numPlantsDiv = document.getElementById("numPlants")
let numWatchersDiv = document.getElementById("numWatchers")
let gardenDiv = document.getElementById("garden")
let tooltipDiv = document.getElementById("tooltip")

let updateState =
    fun newState ->
        match newState with
        | Active update ->
            if update.Tick <= lastTick && not update.ForceReset then
                printfn "Received a stale update (current: %d, update: %d)" lastTick update.Tick
            else
                lastTick <- update.Tick
                ageDiv.innerText <- sprintf "Age: %d ticks" update.Tick
                numPlantsDiv.innerText <- sprintf "Plants:\n%s" (
                    update.NumPlants
                    |> Seq.map (fun kvp -> sprintf "....%ss: %d" kvp.Key kvp.Value)
                    |> String.concat "\n"
                )
                numWatchersDiv.innerText <- sprintf "Watchers: %d" update.NumWatchers
                gardenDiv.innerText <- update.Garden
                printfn "Update: %A" (JSON.stringify(update.HoveredTileInfo))

                tooltipDiv.innerText <-
                    match update.HoveredTileInfo with
                    | Some hti ->
                        sprintf "Tile at %A: %s"
                            hti.Position
                            (
                                match (hti.Tile, hti.Plant) with
                                | (Types.Soil soil, Some plant) ->
                                    printfn "Nitrogen: %A" soil.Nitrogen
                                    sprintf "%s\nNitrogen: %d"
                                        (plant.Type.ToString().ToLower())
                                        (int soil.Nitrogen)
                                | (Types.Soil soil, None) ->
                                    sprintf "soil\nNitrogen: %d" (int soil.Nitrogen)
                                | (Types.Stone, _) -> "stone"
                            )
                    | None -> "Hover over a tile to show tooltip information"
        | Disconnected ->
            document.body.classList.add("bg-gray")
            gardenDiv.classList.remove("bg-white")
            ageDiv.innerText <- "DISCONNECTED"
            let reconnect = document.createElement("button")
            reconnect.innerText <- "Reconnect"
            reconnect.onclick <- fun _ ->
                window.location.reload()
            ageDiv.appendChild(reconnect) |> ignore<Types.Node>

async {
    let! config = remoteApi.GetConfig()

    let mutable hoveredTile = None
    let updateMousePos (evt : Types.MouseEvent) =
        hoveredTile <- Some (
            int <| evt.offsetX * float config.GardenWidth / gardenDiv.clientWidth,
            int <| evt.offsetY * float config.GardenHeight / gardenDiv.clientHeight
        )
    gardenDiv.onmouseenter <- updateMousePos
    gardenDiv.onmousemove <- updateMousePos

    gardenDiv.onmouseleave <- fun _ ->
        hoveredTile <- None

    let mutable failures = 0
    let mutable timerKey = 0
    timerKey <-
        setInterval
            (fun () ->
                async {
                    try
                        let! update = remoteApi.GetUpdate(watcherId, lastTick, hoveredTile)
                        failures <- 0

                        // timerKey = 0 means we've disconnected
                        if timerKey <> 0 then updateState (Active update)
                    with e ->
                        failures <- failures + 1
                        if failures > 15 then
                            clearInterval timerKey
                            timerKey <- 0
                            updateState Disconnected
                        eprintfn "%s %s" (e.ToString()) e.Message
                } |> Async.StartAsPromise |> ignore<Promise<unit>>)
            config.TickLengthMs
} |> Async.StartAsPromise |> ignore<Promise<unit>>


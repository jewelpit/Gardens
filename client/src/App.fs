module Gardens.Client.App

open System
open System.Text

open Browser
open Fable.Core
open Fable.Core.JS
open Fable.Remoting.Client
open Fable.Core.JS
open System

type State =
    | Active of Types.Update
    | Disconnected

let watcherId =
    let watcherIdFromStorage = localStorage.getItem("watcherId")
    if isNull watcherIdFromStorage then
        let watcherId = Guid.NewGuid().ToString()
        localStorage.setItem("watcherId", watcherId)
        watcherId
    else
        watcherIdFromStorage

printfn "Watcher ID: %A" watcherId

let remoteApi =
    Remoting.createApi()
    |> Remoting.buildProxy<Types.RemoteApi>

let createActionButton name action =
    let ret = {|
        Button = document.getElementById(sprintf "button-%s" name) :?> Types.HTMLButtonElement
        Progress = document.getElementById(sprintf "progress-%s" name) :?> Types.HTMLProgressElement
    |}
    ret.Button.onclick <- fun _ ->
        remoteApi.TakeAction(watcherId, action) |> Async.StartAsPromise
    ret.Progress.max <- float action.ActionCost
    ret

let mutable lastTick: int64 = 0L
let ageDiv = document.getElementById("age")
let numPlantsDiv = document.getElementById("numPlants")
let numWatchersDiv = document.getElementById("numWatchers")
let gardenDiv = document.getElementById("garden")
let buttonSow = createActionButton "sow" Types.SowSeeds
let buttonFertilize = createActionButton "fertilize" Types.Fertilize
let tooltipDiv = document.getElementById("tooltip")

let updateState =
    fun newState ->
        match newState with
        | Active update ->
            if update.ForceReset then
                window.location.reload()
            elif update.Tick <= lastTick then
                printfn "Received a stale update (current: %d, update: %d)" lastTick update.Tick
            else
                lastTick <- update.Tick
                gardenDiv.innerText <- update.Garden

                ageDiv.innerText <- sprintf "Age: %d ticks" update.Tick
                numPlantsDiv.innerText <- sprintf "Plants:\n%s" (
                    update.NumPlants
                    |> Seq.map (fun kvp -> sprintf "....%ss: %d" kvp.Key kvp.Value)
                    |> String.concat "\n"
                )
                numWatchersDiv.innerText <- sprintf "Watchers: %d" update.NumWatchers

                buttonSow.Button.disabled <- update.GardenPoints < (int64 Types.SowSeeds.ActionCost)
                buttonSow.Progress.value <- float update.GardenPoints

                buttonFertilize.Button.disabled <- update.GardenPoints < (int64 Types.Fertilize.ActionCost)
                buttonFertilize.Progress.value <- float update.GardenPoints

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


module Gardens.Client.App

open System

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

let updateState =
    let mutable lastTick: int64 = 0L
    let ageDiv = document.getElementById("age")
    let numPlantsDiv = document.getElementById("numPlants")
    let numWatchersDiv = document.getElementById("numWatchers")
    let gardenDiv = document.getElementById("garden")

    fun newState ->
        match newState with
        | Active update ->
            if update.Tick <= lastTick then
                printfn "Received a stale update (current: %d, update: %d)" lastTick update.Tick
            else
                lastTick <- update.Tick
                ageDiv.innerText <- sprintf "Age: %d ticks" update.Tick
                numPlantsDiv.innerText <- sprintf "Plants: %d" update.NumPlants
                numWatchersDiv.innerText <- sprintf "Watchers: %d" update.NumWatchers
                gardenDiv.innerText <- update.Garden
        | Disconnected ->
            document.body.setAttribute("style", "background: lightgrey")
            ageDiv.innerText <- "DISCONNECTED"
            let reconnect = document.createElement("button")
            reconnect.innerText <- "Reconnect"
            reconnect.onclick <- fun _ ->
                window.location.reload()
            ageDiv.appendChild(reconnect) |> ignore<Types.Node>

let mutable failures = 0
let mutable timerKey = 0
timerKey <-
    setInterval
        (fun () ->
            async {
                try
                    let! update = remoteApi.GetUpdate(watcherId)
                    failures <- 0
                    updateState (Active update)
                with e ->
                    failures <- failures + 1
                    if failures > 15 then
                        clearInterval timerKey
                        updateState Disconnected
                    eprintfn "%s %s" (e.ToString()) e.Message
            } |> Async.StartAsPromise |> ignore<Promise<unit>>)
        125

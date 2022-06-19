module Gardens.Client.App

open System

open Browser
open Fable.Core
open Fable.Core.JS
open Fable.Remoting.Client

[<Literal>]
let TickLengthMs = 125

type State =
    | Active of Types.Update
    | Disconnected

let watcherId = Guid.NewGuid().ToString()
printfn "%A" watcherId

let remoteApi =
    Remoting.createApi()
    |> Remoting.buildProxy<Types.RemoteApi>

let mutable lastTick: int64 = 0L
let updateState =
    let ageDiv = document.getElementById("age")
    let numPlantsDiv = document.getElementById("numPlants")
    let numWatchersDiv = document.getElementById("numWatchers")
    let gardenDiv = document.getElementById("garden")

    fun newState ->
        match newState with
        | Active update ->
            if update.Tick <= lastTick && not update.ForceReset then
                printfn "Received a stale update (current: %d, update: %d)" lastTick update.Tick
            else
                lastTick <- update.Tick
                ageDiv.innerText <- sprintf "Age: %d ticks" update.Tick
                numPlantsDiv.innerText <- sprintf "Plants: %s" (
                    update.NumPlants
                    |> Seq.map (fun kvp -> sprintf "%d %ss" kvp.Value (kvp.Key.ToLower()))
                    |> String.concat ", "
                )
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
                    let! update = remoteApi.GetUpdate(watcherId, lastTick)
                    failures <- 0
                    updateState (Active update)
                with e ->
                    failures <- failures + 1
                    if failures > 15 then
                        clearInterval timerKey
                        updateState Disconnected
                    eprintfn "%s %s" (e.ToString()) e.Message
            } |> Async.StartAsPromise |> ignore<Promise<unit>>)
        TickLengthMs

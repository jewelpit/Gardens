module Gardens.Model

open System

open Client.Types

[<Literal>]
let TickLengthMs = 125L

let private random = Random.Shared

// You can only ever accumulate 3x the most expensive action in points.
let private maxPoints =
    [SowSeeds; Fertilize]
    |> List.fold (fun cur action -> max cur (action.ActionCost * 3)) 0
    |> int64

// Hee hee
type GardenState = {
    Tick : int64
    Plants : Map<struct (int * int), Plant>
    NumPlants : Map<PlantType, int>
    Garden : Lazy<string>
    NumWatchers : int
}

type Command =
    | GetState of {|
            HoveredTile : Option<int * int>
            ReplyChannel : AsyncReplyChannel<GardenState * Option<HoveredTileInfo> * int64>
        |}
    | TakeAction of Client.Types.WatcherAction

type ActorMessage = {
    WatcherId : string
    Command : Command
}

[<Struct>]
type WatcherState = {
    LastTick : int64

    // Current tick - points base = points
    PointsBase : int64
}

type Garden(width, height) =
    let garden =
        let garden =
            Array.init height (fun _ ->
                Array.init width (fun _ ->
                    Soil {| Nitrogen = float (random.Next(5_000) + 17_500) |}))
        let numStones = int (float ((random.Next(15) + 5)  * width * height) / 100.0)
        let mutable stoneSeeds = Set.empty
        let startingStoneSeedsCount = random.Next(30) + 31
        while Set.count stoneSeeds < startingStoneSeedsCount do
            stoneSeeds <- Set.add (struct (random.Next() % width, random.Next() % height)) stoneSeeds
        for _ = 1 to numStones do
            let toAdd =
                stoneSeeds
                |> Set.toSeq
                |> Seq.skip (random.Next(Set.count stoneSeeds))
                |> Seq.head
            stoneSeeds <- Set.remove toAdd stoneSeeds
            let struct (x, y) = toAdd
            garden.[y].[x] <- Stone
            for y = max 0 (y - 1) to min (height - 1) (y + 1) do
                for x = max 0 (x - 1) to min (width - 1) (x + 1) do
                    if garden.[y].[x].CanGrowPlants() then
                        stoneSeeds <- Set.add (struct (x, y)) stoneSeeds
        garden

    let tileAt x y =
        garden.[y].[x]

    let spawnPlants makePlant numPlants (maxFailures : Option<int>) curTick plants =
        let rec placePlant plants planted failed =
            if planted >= numPlants || (maxFailures.IsSome && failed >= maxFailures.Value) then
                plants
            else
                let x = random.Next(width)
                let y = random.Next(height)
                let plantType = if random.NextDouble() < 0.75 then Flower else Pea
                if (tileAt x y).CanGrowPlants() then
                    placePlant
                        (Map.add
                            (struct (x, y))
                            (makePlant plantType curTick)
                            plants)
                        (planted + 1)
                        failed
                else
                    placePlant plants planted (failed + 1)
        placePlant plants 0 0

    let displayGarden plants =
        let sb = Text.StringBuilder()
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                match Map.tryFind (struct (x, y)) plants with
                | Some p ->
                    match p.Stage with
                    | Adult _ -> sb.Append(p.Type.Appearance)
                    | Seed _ -> sb.Append(p.Type.SeedAppearance)
                | None ->
                    match tileAt x y with
                    | Soil _ -> sb.Append(" ")
                    | Stone -> sb.Append("O")
                |> ignore<Text.StringBuilder>
            sb.Append("\r\n") |> ignore<Text.StringBuilder>
        sb.ToString()

    let updateSoil plants =
        for KeyValue(struct (x, y), plant) in plants do
            if plant.Stage = Seed then () else
            let spread = plant.Type.NitrogenSpread
            for y' = max 0 (y - spread) to min (height - 1) (y + spread) do
                for x' = max 0 (x - spread) to min (width - 1) (x + spread) do
                    match tileAt x' y' with
                    | Stone -> ()
                    | Soil soil ->
                        let distance = Math.Sqrt(float (y' - y) ** 2.0 + float (x' - x) ** 2.0)
                        if ceil distance <= spread then
                            let nitrogenUsage = plant.Type.NitrogenUsagePerTick / (distance + 1.0)
                            let newNitrogen = max 0.0 (soil.Nitrogen - nitrogenUsage)
                            if newNitrogen <> soil.Nitrogen then
                                garden.[y'].[x'] <- Soil {| soil with Nitrogen = newNitrogen |}

    let sowSeeds curTick plants =
        spawnPlants
            (fun plantType curTick ->
                {
                    Type = plantType
                    PlantedAt = curTick
                    Stage = Seed
                })
            15
            (Some 30)
            curTick
            plants

    let fertilizeSoil () =
        for y = 0 to height - 1 do
            for x = 0 to width - 1 do
                match tileAt x y with
                | Soil s ->
                    garden.[y].[x] <-
                        Soil {| s with Nitrogen = s.Nitrogen + float Flower.MinNitrogen |}
                | Stone -> ()

    let canGrow (plantType : PlantType) x y =
        match tileAt x y with
        | Soil soil ->
            random.Next(plantType.MinNitrogen) <= int soil.Nitrogen
                && random.Next(int soil.Nitrogen) <= plantType.MaxNitrogen
        | Stone -> false

    let updatePlants plants tick =
        Map.fold (fun (struct (numPlants, plants)) pos plant ->
            let struct (x, y) = pos
            let plants =
                match plant.Stage with
                | Seed ->
                    if plant.PlantedAt + 250L < tick then
                        if canGrow plant.Type x y then
                            Map.add pos { plant with Stage = Adult (struct {| LastSeedAt = tick |})} plants
                        else
                            // Nothing personnel kid
                            Map.remove pos plants
                    else
                        plants
                | Adult a ->
                    if random.Next(max (int (plant.Type.MaxAge - (tick - plant.PlantedAt))) 100) = 0 then
                        Map.remove pos plants
                    elif a.LastSeedAt + plant.Type.SeedCooldown < tick && random.Next(100) = 0 then
                        // Nitrogen deficiency causes seed production to age a plant at a faster rate.
                        let ageMod = if canGrow plant.Type x y then 1000L else 0L
                        let plants =
                            Map.add
                                pos
                                { plant with
                                    Stage = Adult (struct {| LastSeedAt = tick |})
                                    PlantedAt = plant.PlantedAt - ageMod
                                }
                                plants
                        let radians = random.NextDouble() * 2.0 * Math.PI
                        let distance = random.NextDouble() * plant.Type.SeedDistance + 1.0
                        let dx = Math.Cos(radians) * distance
                        let dy = Math.Sin(radians) * distance
                        let x = int (Math.Round(float x + dx))
                        let y = int (Math.Round(float y + dy))
                        let inBounds = x >= 0 && x < width && y >= 0 && y < height
                        let inSoil = inBounds && (tileAt x y).CanGrowPlants()
                        let empty = not (Map.containsKey (struct (x, y)) plants)
                        if inSoil && empty then
                            Map.add (struct (x, y)) { Type = plant.Type; PlantedAt = tick; Stage = Seed } plants
                        else
                            plants
                    else
                        plants
            // This is always going to be one tick behind when counting deaths, but that's fine.
            let plantTypeCount = Map.tryFind plant.Type numPlants |> Option.defaultValue 0
            let numPlants = Map.add plant.Type (plantTypeCount + 1) numPlants
            struct (numPlants, plants)
        ) (struct (Map.empty, plants)) plants

    let handleTick state =
        let tick = state.Tick + 1L
        updateSoil state.Plants
        let struct (numPlants, plants) = updatePlants state.Plants tick
        {
            Tick = tick
            Plants = plants
            NumPlants = numPlants
            Garden = Lazy<string>.Create(fun () -> displayGarden plants)
            NumWatchers = state.NumWatchers
        }

    let updateWatchers newState watchers watcherId =
        let watcherState =
            match Map.tryFind watcherId watchers with
            | None -> { LastTick = newState.Tick; PointsBase = newState.Tick }
            | Some ws -> { ws with LastTick = newState.Tick }
        let gardenPoints = newState.Tick - watcherState.PointsBase
        let watchers = Map.add watcherId watcherState watchers
        let (struct (watchers, numWatchers)) =
            Map.fold (fun (struct (watchers, numWatchers)) watcherId watcherState ->
                if newState.Tick - watcherState.LastTick > 15L then
                    struct (Map.remove watcherId watchers, numWatchers)
                elif watcherId = "" then
                    struct (watchers, numWatchers)
                else
                    struct (watchers, numWatchers + 1)
            ) (struct (watchers, 0)) watchers
        if numWatchers <> newState.NumWatchers then
            struct ({ newState with NumWatchers = numWatchers }, watchers, gardenPoints)
        else
            struct (newState, watchers, gardenPoints)

    let mailbox = MailboxProcessor<ActorMessage>.Start(fun inbox ->
        let creationStopwatch = Diagnostics.Stopwatch.StartNew()
        let rec messageLoop baseTicks watchers cachedState = async {
            let! message = inbox.Receive()
            let watcherId = message.WatcherId
            match message.Command with
            | GetState gs ->
                let (hoveredTile, replyChannel) = (gs.HoveredTile, gs.ReplyChannel)
                let ticks = baseTicks + creationStopwatch.ElapsedMilliseconds / TickLengthMs
                let lastTick = cachedState.Tick
                let (baseTicks, newState) =
                    if ticks <> lastTick then
                        let elapsedTicks = min 10L (ticks - lastTick)
                        let baseTicks =
                            if elapsedTicks <> ticks - lastTick then
                                creationStopwatch.Restart()
                                lastTick + elapsedTicks
                            else baseTicks
                        let rec advance state =
                            if state.Tick = lastTick + elapsedTicks then
                                state
                            else
                                advance ( state)
                        (baseTicks, handleTick cachedState)
                    else (baseTicks, cachedState)
                let struct (newState, watchers, gardenPoints) = updateWatchers newState watchers watcherId
                let gardenPoints = min gardenPoints maxPoints
                let hoveredTileInfo =
                    hoveredTile
                    |> Option.map (fun (x, y) ->
                        let x = max (min (width - 1) x) 0
                        let y = max (min (height - 1) y) 0
                        {
                            Position = (x, y)
                            Tile = tileAt x y
                            Plant = Map.tryFind (struct (x, y)) newState.Plants
                        })
                replyChannel.Reply((newState, hoveredTileInfo, gardenPoints))
                return! messageLoop baseTicks watchers newState
            | TakeAction a ->
                let (watchers, newState) =
                    match Map.tryFind message.WatcherId watchers with
                    | Some ws when cachedState.Tick - ws.PointsBase >= a.ActionCost ->
                        let newState =
                            match a with
                            | SowSeeds ->
                                { cachedState with
                                    Plants = sowSeeds cachedState.Tick cachedState.Plants
                                }
                            | Fertilize ->
                                fertilizeSoil ()
                                cachedState
                        let watchers =
                            Map.add
                                message.WatcherId
                                { ws with
                                    PointsBase =
                                        max
                                            (ws.PointsBase + (int64 a.ActionCost))
                                            (cachedState.Tick - maxPoints)
                                }
                                watchers
                        (watchers, newState)
                    | _ -> (watchers, cachedState)
                return! messageLoop baseTicks watchers newState
        }

        let makePlant (plantType : PlantType) curTick =
            let lastSeedAt = curTick - (int64 (random.NextInt64(plantType.SeedCooldown)))
            {
                Type = plantType
                PlantedAt = curTick
                Stage = Adult (struct {| LastSeedAt = lastSeedAt |})
            }
        let numPlants = int (float ((random.Next(15) + 5)  * width * height) / 100.0)
        let plants = spawnPlants makePlant numPlants None 0L Map.empty
        messageLoop 0L Map.empty {
            Tick = 0L
            Plants = plants
            NumPlants = Map.empty
            Garden = Lazy<string>.CreateFromValue("")
            NumWatchers = 0
        }
    )

    let getState watcherId hoveredTile =
        mailbox.PostAndAsyncReply(fun channel ->
            {
                WatcherId = watcherId
                Command = GetState {|
                    HoveredTile = hoveredTile
                    ReplyChannel = channel
                |}
            })

    let idler =
        let rec helper () = async {
            if mailbox.CurrentQueueLength = 0 then
                let! _ = getState "" None
                ()
            do! Async.Sleep (TimeSpan.FromSeconds(1))
            return! helper ()
        }
        helper () |> Async.StartAsTask

    member __.Width with get () =
        width

    member __.Height with get () =
        height

    member __.GetState(watcherId, hoveredTile) =
        getState watcherId hoveredTile

    member __.TakeAction(watcherId, action) =
        mailbox.Post({ WatcherId = watcherId; Command = TakeAction action })

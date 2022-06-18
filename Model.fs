module Gardens.Model

open System

[<DefaultAugmentation(false)>]
[<Struct>]
type Tile =
    | Soil of struct {| Nitrogen: float |}
    | Stone

    member this.IsSoil() =
        match this with
        | Soil _ -> true
        | _ -> false

type PlantType =
    | Flower

    member this.SeedCooldown with get () =
        match this with
        | Flower -> 1000L

    member this.MaxAge with get () =
        match this with
        | Flower -> 10000L

    /// When the nitrogen falls below this level, plant changes (such as moving to adulthood or
    /// releasing seeds) have a chance to result in a negative outcome.
    member this.NitrogenBaseline with get () =
        match this with
        | Flower -> 6000

    member this.NitrogenUsagePerTick with get () =
        match this with
        | Flower -> 1.0

type PlantStage =
    | Seed
    | Adult of struct {| LastSeedAt: int64 |}

type Plant = {
    Type : PlantType
    PlantedAt: int64
    Stage : PlantStage
}

// Hee hee
type GardenState = {
    Tick : int64
    Plants : Map<struct (int * int), Plant>
    NumPlants : int // Map<> calculates size lazily, so cache it
    Garden : Lazy<string>
    NumWatchers : int
}

type Garden(width, height) =
    let random = Random()

    let garden =
        let garden =
            Array.init height (fun _ ->
                Array.init width (fun _ ->
                    Soil {| Nitrogen = (random.NextDouble() * 5000.0) + 7500.0 |}))
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
                    if garden.[y].[x].IsSoil() then
                        stoneSeeds <- Set.add (struct (x, y)) stoneSeeds
        garden

    let tileAt x y =
        garden.[y].[x]

    let spawnPlants () =
        let numPlants = int (float ((random.Next(15) + 5)  * width * height) / 100.0)
        let rec placePlant plants =
            if Map.count plants >= numPlants then
                plants
            else
                let x = random.Next(width)
                let y = random.Next(height)
                if (tileAt x y).IsSoil() then
                    let lastSeedAt = 0L - (int64 (random.NextInt64(Flower.SeedCooldown)))
                    placePlant
                        (Map.add
                            (struct (x, y))
                            {
                                Type = Flower
                                PlantedAt = 0L
                                Stage = Adult (struct {| LastSeedAt = lastSeedAt |})
                            }
                            plants)
                else
                    placePlant plants
        placePlant Map.empty

    let displayGarden plants =
        let sb = Text.StringBuilder()
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                match Map.tryFind (struct (x, y)) plants with
                | Some p ->
                    match p.Stage with
                    | Adult _ -> sb.Append("%")
                    | Seed _ -> sb.Append(".")
                | None -> if garden.[y].[x].IsSoil() then sb.Append(" ") else sb.Append("O")
                |> ignore<Text.StringBuilder>
            sb.Append("\r\n") |> ignore<Text.StringBuilder>
        sb.ToString()

    let updateSoil plants =
        for KeyValue(struct (x, y), plant) in plants do
            for y' = max 0 (y - 2) to min (height - 1) (y + 2) do
                for x' = max 0 (x - 2) to min (width - 1) (x + 2) do
                match tileAt x y with
                | Stone -> ()
                | Soil soil ->
                    let distance = Math.Sqrt(float (y' - y) ** 2.0 + float (x' - x) ** 2.0)
                    let nitrogenUsage = plant.Type.NitrogenUsagePerTick / (distance + 1.0)
                    let newNitrogen = max 0.0 (soil.Nitrogen - nitrogenUsage)
                    if newNitrogen <> soil.Nitrogen then
                        garden.[y].[x] <- Soil {| soil with Nitrogen = newNitrogen |}

    let canGrow (plantType : PlantType) x y =
        match tileAt x y with
        | Soil soil ->
            soil.Nitrogen >= random.Next(plantType.NitrogenBaseline)
        | Stone -> false

    let updatePlants plants tick =
        Map.fold (fun (struct (count, plants)) pos plant ->
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
                        if canGrow plant.Type x y then
                            let plants = Map.add pos { plant with Stage = Adult (struct {| LastSeedAt = tick |}) } plants
                            let radians = random.NextDouble() * 2.0 * Math.PI
                            let distance = random.NextDouble() * 10.0 + 1.0
                            let dx = Math.Cos(radians) * distance
                            let dy = Math.Sin(radians) * distance
                            let x = int (Math.Round(float x + dx))
                            let y = int (Math.Round(float y + dy))
                            let inBounds = x >= 0 && x < width && y >= 0 && y < height
                            let inSoil = inBounds && (tileAt x y).IsSoil()
                            let empty = not (Map.containsKey (struct (x, y)) plants)
                            if inSoil && empty then
                                Map.add (struct (x, y)) { Type = Flower; PlantedAt = tick; Stage = Seed } plants
                            else
                                plants
                        else
                            // Age the plant one hundred ticks instead of releasing a seed.
                            Map.add
                                (struct (x, y)) { plant with PlantedAt = plant.PlantedAt - 100L }
                                plants
                    else
                        plants
            struct (count + 1, plants)
        ) (struct (0, plants)) plants

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
        let watchers = Map.add watcherId newState.Tick watchers
        let (struct (watchers, numWatchers)) =
            Map.fold (fun (struct (watchers, numWatchers)) watcherId lastQueryTick ->
                if newState.Tick - lastQueryTick > 15L then
                    struct (Map.remove watcherId watchers, numWatchers)
                else
                    struct (watchers, numWatchers + 1)
            ) (struct (watchers, 0)) watchers
        if numWatchers <> newState.NumWatchers then
            struct ({ newState with NumWatchers = numWatchers }, watchers)
        else
            struct (newState, watchers)

    let mailbox = MailboxProcessor<(string * AsyncReplyChannel<GardenState>)>.Start(fun inbox ->
        let creationStopwatch = Diagnostics.Stopwatch.StartNew()
        let rec messageLoop baseTicks watchers cachedState =
            async {
                let! (watcherId, replyChannel) = inbox.Receive()
                let ticks = baseTicks + creationStopwatch.ElapsedMilliseconds / 125L
                let lastTick = cachedState.Tick
                let (baseTicks, newState) =
                    if ticks <> lastTick then
                        let elapsedTicks = min 1000L (ticks - lastTick)
                        let baseTicks =
                            if elapsedTicks <> ticks - lastTick then
                                creationStopwatch.Restart()
                                lastTick + elapsedTicks
                            else baseTicks
                        let rec advance state =
                            if state.Tick = lastTick + elapsedTicks then state else handleTick state
                        (baseTicks, advance cachedState)
                    else (baseTicks, cachedState)
                let struct (newState, watchers) = updateWatchers newState watchers watcherId
                replyChannel.Reply(newState)
                return! messageLoop baseTicks watchers newState
            }
        let plants = spawnPlants ()
        messageLoop 0L Map.empty {
            Tick = 0L
            Plants = plants
            NumPlants = (Map.count plants)
            Garden = Lazy<string>.CreateFromValue("")
            NumWatchers = 0
        }
    )

    member __.GetState(watcherId) =
        Async.StartAsTask (mailbox.PostAndAsyncReply(fun channel -> (watcherId, channel)))

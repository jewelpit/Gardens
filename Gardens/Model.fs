module Gardens.Model

open System

[<Literal>]
let SeedCooldown = 1000L

type Tile =
    | Soil
    | Stone

type PlantType =
    | Flower

type PlantAge =
    | Seed of struct {| Planted: int64 |}
    | Adult of struct {| LastSeedAt: int64 |}

type Plant = {
    Type : PlantType
    Age : PlantAge
}

type Garden(width, height) as this =
    let random = Random()
    let mutable plants = Map.empty

    let mailbox = MailboxProcessor<AsyncReplyChannel<int64>>.Start(fun inbox ->
        let creationStopwatch = Diagnostics.Stopwatch.StartNew()
        let rec messageLoop lastTick =
            async {
                let! replyChannel = inbox.Receive()
                let ticks = creationStopwatch.ElapsedMilliseconds / 125L
                let elapsedTicks = min 1000L (ticks - lastTick)
                for t = 1L to elapsedTicks do
                    this.HandleTick(lastTick + t)
                replyChannel.Reply(ticks)
                return! messageLoop ticks
            }
        messageLoop 0L)

    let garden = Array.init height (fun _ -> Array.create width Soil)
    do
        let numStones = int (float ((random.Next() % 15 + 5)  * width * height) / 100.0)
        let mutable stoneSeeds = Set.empty
        let startingStoneSeedsCount = random.Next() % 30 + 31
        while Set.count stoneSeeds < startingStoneSeedsCount do
            stoneSeeds <- Set.add (struct (random.Next() % width, random.Next() % height)) stoneSeeds
        for _ = 1 to numStones do
            let toAdd =
                stoneSeeds
                |> Set.toSeq
                |> Seq.skip (random.Next() % (Set.count stoneSeeds))
                |> Seq.head
            stoneSeeds <- Set.remove toAdd stoneSeeds
            let struct (x, y) = toAdd
            garden.[y].[x] <- Stone
            for y = max 0 (y - 1) to min (height - 1) (y + 1) do
                for x = max 0 (x - 1) to min (width - 1) (x + 1) do
                    if garden.[y].[x] = Soil then
                        stoneSeeds <- Set.add (struct (x, y)) stoneSeeds

        let numPlants = int (float ((random.Next() % 15 + 5)  * width * height) / 100.0)
        let rec placePlant plants =
            if Map.count plants >= numPlants then
                plants
            else
                let x = random.Next() % width
                let y = random.Next() % height
                if this.TileAt(x, y) = Soil then
                    placePlant
                        (Map.add
                            (struct (x, y))
                            {
                                Type = Flower;
                                Age = Adult (struct {| LastSeedAt = 0L - (int64 (random.Next()) % SeedCooldown) |})
                            }
                            plants)
                else
                    placePlant plants
        plants <- placePlant plants

    member __.Ticks =
        Async.StartAsTask (mailbox.PostAndAsyncReply(id))

    member private __.TileAt(x, y) =
        garden.[y].[x]

    member private __.HandleTick(tick) =
        plants <- Map.fold (fun plants pos plant ->
            match plant.Age with
            | Seed s ->
                if s.Planted + 250L < tick then
                    Map.add pos { plant with Age = Adult (struct {| LastSeedAt = tick |})} plants
                else
                    plants
            | Adult a ->
                if a.LastSeedAt + SeedCooldown < tick && random.Next() % 100 = 0 then
                    let plants = Map.add pos { plant with Age = Adult (struct {| LastSeedAt = tick |}) } plants
                    let radians = random.NextDouble() * 2.0 * Math.PI
                    let distance = random.NextDouble() * 10.0 + 1.0
                    let dx = Math.Cos(radians) * distance
                    let dy = Math.Sin(radians) * distance
                    let struct (x, y) = pos
                    let x = int (Math.Round(float x + dx))
                    let y = int (Math.Round(float y + dy))
                    let inBounds = x >= 0 && x < width - 1 && y >= 0 && y < height - 1
                    let inSoil = inBounds && this.TileAt(x, y) = Soil // Gotta short circuit this one.
                    let empty = not (Map.containsKey (struct (x, y)) plants)
                    if inBounds && inSoil && empty then
                        Map.add (struct (x, y)) { Type = Flower; Age = Seed (struct {| Planted = tick |}) } plants
                    else
                        plants
                else
                    plants
        ) plants plants

    override __.ToString() =
        let sb = Text.StringBuilder()
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                match Map.tryFind (struct (x, y)) plants with
                | Some p ->
                    match p.Age with
                    | Adult _ -> sb.Append("%")
                    | Seed _ -> sb.Append(",")
                | None -> if garden.[y].[x] = Soil then sb.Append(".") else sb.Append("O")
                |> ignore<Text.StringBuilder>
            sb.Append("\r\n") |> ignore<Text.StringBuilder>
        sb.ToString()

type Update = {
    Tick : int64
    Garden : string
}

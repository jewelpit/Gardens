module Gardens.Model

open System

type Tile =
    | Soil
    | Stone

type Plant =
    | Flower

type Garden(width, height) as this =
    let random = Random()
    let mutable plants = Set.empty

    let mailbox = MailboxProcessor<AsyncReplyChannel<int64>>.Start(fun inbox ->
        let creationStopwatch = Diagnostics.Stopwatch.StartNew()
        let rec messageLoop lastTick =
            async {
                let! replyChannel = inbox.Receive()
                let ticks = creationStopwatch.ElapsedMilliseconds / 125L
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
            let toAdd = stoneSeeds |> Set.toSeq |> Seq.skip (random.Next() % (Set.count stoneSeeds)) |> Seq.head
            stoneSeeds <- Set.remove toAdd stoneSeeds
            let struct (x, y) = toAdd
            garden.[y].[x] <- Stone
            for y = max 0 (y - 1) to min (height - 1) (y + 1) do
                for x = max 0 (x - 1) to min (width - 1) (x + 1) do
                    if garden.[y].[x] = Soil then
                        stoneSeeds <- Set.add (struct (x, y)) stoneSeeds

        let numPlants = int (float ((random.Next() % 15 + 5)  * width * height) / 100.0)
        let rec placePlant plants =
            if Set.count plants >= numPlants then
                plants
            else
                let x = random.Next() % width
                let y = random.Next() % height
                if this.TileAt(x, y) = Soil then
                    placePlant (Set.add (struct (x, y)) plants)
                else
                    placePlant plants
        plants <- placePlant plants

    member private __.TileAt(x, y) =
        garden.[y].[x]

    member __.Ticks =
        Async.StartAsTask (mailbox.PostAndAsyncReply(id, 1500))

    override __.ToString() =
        let sb = Text.StringBuilder()
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                if Set.contains (struct (x, y)) plants then
                    sb.Append("%")
                else
                    if garden.[y].[x] = Soil then sb.Append(".") else sb.Append("O")
                |> ignore<Text.StringBuilder>
            sb.Append("\r\n") |> ignore<Text.StringBuilder>
        sb.ToString()

type Update = {
    Tick : int64
}

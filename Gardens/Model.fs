module Gardens.Model

open System
open FSharp.Control.Tasks.Affine

type Garden(name : string) =
    let mutable ticks = 0L
    let mailbox = MailboxProcessor.Start(fun inbox ->
        let rec messageLoop () =
            async {
                let! () = inbox.Receive()
                ticks <- ticks + 1L
                return! messageLoop ()
            }
        messageLoop ())

    let garden =
        RogueSharp.Map.Create(
            RogueSharp.MapCreation.CaveMapCreationStrategy(
                80,
                50,
                75,
                50,
                500,
                RogueSharp.Random.DotNetRandom()))

    member __.Name = name
    member __.Ticks = ticks

    member __.Tick() =
        if mailbox.CurrentQueueLength > 2 then
            ()
        else
            mailbox.Post()

    override __.ToString() =
        let sb = Text.StringBuilder()
        for y in 0 .. (garden.Height - 1) do
            for x in 0 .. (garden.Width - 1) do
                if garden.GetCell(x, y).IsWalkable then sb.Append(" ") else sb.Append("O")
                |> ignore<Text.StringBuilder>
            sb.Append("\r\n") |> ignore<Text.StringBuilder>
        sb.ToString()


type GardenManager() =
    let mutable nextId = 0L
    let mutable gardens : Map<int64, Garden> = Map.empty
    let lock = new Utils.Guard()
    let timer = new Timers.Timer(20.0)
    do
        timer.Elapsed.Add(fun _ ->
            gardens
            |> Map.iter (fun _ garden -> garden.Tick()))
        timer.AutoReset <- true
        timer.Enabled <- true

    interface IDisposable with
        member __.Dispose() =
            (lock :> IDisposable).Dispose()
            (timer :> IDisposable).Dispose()

    member __.Gardens = gardens

    member __.AddGarden() =
        task {
            use! _guard = lock.Lock()
            let id = nextId
            let name = sprintf "Garden %d" id
            nextId <- nextId + 1L
            gardens <- Map.add id (Garden(name)) gardens
        }

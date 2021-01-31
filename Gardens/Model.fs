module Gardens.Model

open System

type Garden() =
    let mutable ticks = 0L
    let mailbox = MailboxProcessor.Start(fun inbox ->
        let rec messageLoop () =
            async {
                let! () = inbox.Receive()
                ticks <- ticks + 1L
                return! messageLoop ()
            }
        messageLoop ())
    let timer = new Timers.Timer(50.0)
    do
        timer.Elapsed.Add(fun _ ->
            if mailbox.CurrentQueueLength > 2 then
                ()
            else
                mailbox.Post())
        timer.AutoReset <- true
        timer.Enabled <- true

    let garden =
        RogueSharp.Map.Create(
            RogueSharp.MapCreation.CaveMapCreationStrategy(
                80,
                50,
                75,
                50,
                500,
                RogueSharp.Random.DotNetRandom()))

    member __.Ticks = ticks

    override __.ToString() =
        let sb = Text.StringBuilder()
        for y in 0 .. (garden.Height - 1) do
            for x in 0 .. (garden.Width - 1) do
                if garden.GetCell(x, y).IsWalkable then sb.Append(" ") else sb.Append("O")
                |> ignore<Text.StringBuilder>
            sb.Append("\r\n") |> ignore<Text.StringBuilder>
        sb.ToString()

type Update = {
    Tick : int64
}

module Gardens.Model

open System

type Garden() =
    let mailbox = MailboxProcessor<AsyncReplyChannel<int64>>.Start(fun inbox ->
        let creationStopwatch = Diagnostics.Stopwatch.StartNew()
        let rec messageLoop () =
            async {
                let! replyChannel = inbox.Receive()
                let ticks = creationStopwatch.ElapsedMilliseconds / 125L
                replyChannel.Reply(ticks)
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

    member __.Ticks =
        Async.StartAsTask (mailbox.PostAndAsyncReply(id, 1500))

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

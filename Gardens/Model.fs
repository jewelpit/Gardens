module Gardens.Model

open System
open FSharp.Control.Tasks.Affine

type Garden = {
    Name : string
}

type GardenManager() =
    let mutable nextId = 0L
    let mutable gardens : Map<int64, Garden> = Map.empty
    let lock = new Utils.Guard()

    interface IDisposable with
        member __.Dispose() =
            (lock :> IDisposable).Dispose()

    member __.Gardens = gardens

    member __.AddGarden() =
        task {
            use! _guard = lock.Lock()
            let id = nextId
            let name = sprintf "Garden %d" id
            nextId <- nextId + 1L
            gardens <- Map.add id { Name = name } gardens
        }

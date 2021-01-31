module Gardens.Utils

open System
open FSharp.Control.Tasks.Affine

type Guard() =
    let lock = new Threading.SemaphoreSlim(1)

    interface IDisposable with
        member __.Dispose() =
            lock.Dispose()

    member __.Lock() =
        task {
            do! lock.WaitAsync()
            return {
                new System.IDisposable with
                    member __.Dispose() =
                        lock.Release() |> ignore<int>
            }
        }
module Gardens.Client.Types

type Update = {
    Tick : int64
    NumPlants : int
    Garden : string
    NumWatchers : int
}

type RemoteApi = {
    GetUpdate : string -> Async<Update>
}
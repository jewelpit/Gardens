module Gardens.Client.Types

type Update = {
    Tick : int64
    NumPlants : Map<string, int>
    Garden : string
    NumWatchers : int
    ForceReset : bool
}

type RemoteApi = {
    GetUpdate : (string * int64) -> Async<Update>
}
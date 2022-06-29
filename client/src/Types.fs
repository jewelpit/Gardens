module Gardens.Client.Types

type Config = {
    TickLengthMs : int
    GardenWidth : int
    GardenHeight : int
}

[<Struct>]
type Tile =
    | Soil of {| Nitrogen: float |}
    | Stone

    member this.CanGrowPlants() =
        match this with
        | Soil _ -> true
        | _ -> false

type PlantType =
    | Flower
    | Pea

    member this.Appearance with get () =
        match this with
        | Flower -> '%'
        | Pea -> 'ſ'

    member this.SeedAppearance with get () =
        match this with
        | Flower -> '.'
        | Pea -> ','

    member this.SeedCooldown with get () =
        match this with
        | Flower -> 750L
        | Pea -> 3_000L

    member this.SeedDistance with get () =
        match this with
        | Flower -> 20.0
        | Pea -> 5.0

    member this.MaxAge with get () =
        match this with
        | Flower -> 10_000L
        | Pea -> 20_000L

    member this.MinNitrogen with get () =
        match this with
        | Flower -> 5_000
        | Pea -> 0

    member this.MaxNitrogen with get () =
        match this with
        | Flower -> 250_000
        | Pea -> 100_000

    member this.NitrogenSpread with get () =
        match this with
        | Flower -> 2
        | Pea -> 5

    member this.NitrogenUsagePerTick with get () =
        match this with
        | Flower -> 1.0
        | Pea -> -5.0

type PlantStage =
    | Seed
    | Adult of struct {| LastSeedAt: int64 |}

type Plant = {
    Type : PlantType
    PlantedAt: int64
    Stage : PlantStage
}

type HoveredTileInfo = {
    Position : (int * int)
    Tile : Tile
    Plant : Option<Plant>
}

type WatcherAction =
    | SowSeeds
    | Fertilize

    member this.ActionCost with get () =
        match this with
        | SowSeeds -> 1_000
        | Fertilize -> 3_500

type Update = {
    Tick : int64
    NumPlants : Map<string, int>
    Garden : string
    NumWatchers : int
    HoveredTileInfo : Option<HoveredTileInfo>
    GardenPoints : int64
    ForceReset : bool
}

type RemoteApi = {
    GetConfig : unit -> Async<Config>
    GetUpdate : (string * int64 * Option<(int * int)>) -> Async<Update>
    TakeAction : (string * WatcherAction) -> Async<unit>
}
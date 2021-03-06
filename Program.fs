module Gardens.App

open System
open System.IO
open System.Reflection
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open FSharp.Control.Tasks.Affine
open Giraffe

open Gardens.Client.Types

let (htmlResource, jsResource) =
    let serveResource (handler : string -> HttpHandler) (resourceName : string) =
        let assembly = Assembly.GetExecutingAssembly()

        let resource =
            task {
                use reader =
                    new StreamReader(assembly.GetManifestResourceStream($"Gardens.{resourceName}"))

                return! reader.ReadToEndAsync()
            }

        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! resource = resource
                return! (handler resource) next ctx
            }

    let sendContent contentType str =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            ctx.SetContentType contentType
            ctx.WriteStringAsync str

    (serveResource htmlString, serveResource (sendContent "text/javascript"))

let webApp (garden : Model.Garden) =
    let getConfig () =
        async {
            return {
                TickLengthMs = int Model.TickLengthMs
                GardenWidth = garden.Width
                GardenHeight = garden.Height
            }
        }

    let getUpdate (watcherId, lastTick, hoveredTile) =
        async {
            let! (state, hoveredTileInfo, points) = garden.GetState(watcherId, hoveredTile)

            return {
                Tick = state.Tick
                NumPlants =
                    state.NumPlants
                    |> Seq.map (fun kvp -> (kvp.Key.ToString(), kvp.Value))
                    |> Map.ofSeq
                Garden = state.Garden.Value
                NumWatchers = state.NumWatchers
                HoveredTileInfo = hoveredTileInfo
                GardenPoints = points
                ForceReset = lastTick > state.Tick
            }
        }

    let takeAction (watcherId, action) =
        async { garden.TakeAction(watcherId, action) }

    choose [
        GET >=> choose [
            route "/" >=> htmlResource "client.public.index.html"
            route "/bundle.js" >=> jsResource "client.public.bundle.js"
        ]
        Remoting.createApi()
            |> Remoting.withErrorHandler (fun ex _ -> Propagate ex)
            |> Remoting.fromValue { GetConfig = getConfig; GetUpdate = getUpdate; TakeAction = takeAction }
            |> Remoting.buildHttpHandler
        setStatusCode 404 >=> text "Not Found"
    ]

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")

    clearResponse
    >=> setStatusCode 500
    >=> text ex.Message

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins("http://localhost:5000", "https://localhost:5001", "https://gardens.robo.church")
        .AllowAnyMethod()
        .AllowAnyHeader()
    |> ignore<CorsPolicyBuilder>

let configureApp (app : IApplicationBuilder) =
    let garden = Model.Garden(80, 50)
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()

    let appBuilder =
        match env.IsDevelopment() with
        | true -> app.UseDeveloperExceptionPage()
        | false -> app.UseGiraffeErrorHandler(errorHandler)

    appBuilder
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp garden)

let configureServices (services : IServiceCollection) =
    services.AddCors().AddGiraffe()
    |> ignore<IServiceCollection>

let configureLogging (builder : ILoggingBuilder) =
    builder.AddConsole().AddDebug()
    |> ignore<ILoggingBuilder>

[<EntryPoint>]
let main args =
    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder
                .Configure(Action<IApplicationBuilder> configureApp)
                .ConfigureServices(configureServices)
                .ConfigureLogging(configureLogging)
            |> ignore<IWebHostBuilder>)
        .Build()
        .Run()

    0

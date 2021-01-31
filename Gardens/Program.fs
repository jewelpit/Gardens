module Gardens.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

open FSharp.Control.Tasks.Affine
open Giraffe

let indexHandler (gardenManager : Model.GardenManager) =
    let view = Views.index gardenManager
    htmlView view

let showGarden (gardenManager : Model.GardenManager) (id : int64) =
    fun next ctx ->
        match Map.tryFind id gardenManager.Gardens with
        | Some garden -> htmlView (Views.garden garden) next ctx
        | None -> Threading.Tasks.Task.FromResult None

let addGarden (gardenManager : Model.GardenManager) =
    fun next ctx ->
        task {
            do! gardenManager.AddGarden()
            return! redirectTo false "/" next ctx
        }

let webApp (gardenManager : Model.GardenManager) =
    choose [
        GET >=>
            choose [
                route "/" >=> warbler (fun _ -> indexHandler gardenManager)
                routef "/garden/%d" (fun id -> showGarden gardenManager id)
            ]
        POST >=>
            choose [
                routex "/add_garden(/)?" >=> addGarden gardenManager
            ]
        setStatusCode 404 >=> text "Not Found"
    ]

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001")
       .AllowAnyMethod()
       .AllowAnyHeader()
   |> ignore<CorsPolicyBuilder>

let configureApp (app : IApplicationBuilder) =
    let gardenManager = new Model.GardenManager()
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    let appBuilder =
        match env.IsDevelopment() with
        | true  ->
            app.UseDeveloperExceptionPage()
        | false ->
            app.UseGiraffeErrorHandler(errorHandler)
    appBuilder
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp gardenManager)

let configureServices (services : IServiceCollection) =
    services
        .AddCors()
        .AddGiraffe()
    |> ignore<IServiceCollection>

let configureLogging (builder : ILoggingBuilder) =
    builder
        .AddConsole()
        .AddDebug()
    |> ignore<ILoggingBuilder>

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot = Path.Combine(contentRoot, "WebRoot")
    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                |> ignore<IWebHostBuilder>)
        .Build()
        .Run()
    0
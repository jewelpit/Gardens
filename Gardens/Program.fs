module Gardens.App

open System
open System.IO
open System.Reflection
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

open FSharp.Control.Tasks.Affine
open Giraffe

let indexHandler (garden : Model.Garden) =
    let view = Views.index garden
    htmlView view

let webApp (garden : Model.Garden) =
    choose [
        GET >=>
            choose [
                route "/" >=> warbler (fun _ -> indexHandler garden)
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
    let garden = Model.Garden()
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
        .UseGiraffe(webApp garden)

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
    let contentRoot = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
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
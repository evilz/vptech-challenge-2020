module CodeChallenge.Program

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.Serialization
open System.Text.Json
open System.Text.Json.Serialization
open Giraffe.SerilogExtensions
open Serilog
open Microsoft.Extensions.Logging


let webApp =
    choose [
        POST >=> route "/move" >=> Handlers.moveHandler
        route "/name"   >=> Handlers.nameHandler
        route "/status"   >=> Handlers.nameHandler
        route "/health"   >=> Handlers.nameHandler
        route "/"   >=> Handlers.nameHandler
        RequestErrors.NOT_FOUND "What do you want ???" ]

type Startup() =
    member __.ConfigureServices (services : IServiceCollection) =
        let sp  = services.BuildServiceProvider()
        let env = sp.GetService<IWebHostEnvironment>()
        if env.IsDevelopment()
        then
            Log.Logger <- 
              LoggerConfiguration()
                .MinimumLevel.Debug()
                .Destructure.FSharpTypes()
                .WriteTo.Console()
                .CreateLogger()
            
        else
            Log.Logger <- 
              LoggerConfiguration()
                .MinimumLevel.Debug()
                .Destructure.FSharpTypes()
                .WriteTo.Console(Logging.VeepeeJsonFormatter())
                .CreateLogger()

        // Add Giraffe dependencies
        services.AddGiraffe() |> ignore
        let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
        options.Converters.Add(JsonFSharpConverter(unionEncoding = JsonUnionEncoding.UnwrapFieldlessTags, unionTagNamingPolicy = JsonNamingPolicy.CamelCase))
        services.AddSingleton<IJsonSerializer>(SystemTextJsonSerializer (options)) |> ignore

    member __.Configure (app : IApplicationBuilder)
                        (env : IHostingEnvironment)
                        (loggerFactory : ILoggerFactory) =
        // Add Giraffe to the ASP.NET Core pipeline
        app.UseGiraffe (SerilogAdapter.Enable(webApp))   

let CreateHostBuilder(args:string[] ) = 
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseUrls("http://0.0.0.0:5000")
                    .UseStartup<Startup>()
                    |> ignore)
[<EntryPoint>]
let main args =
    CreateHostBuilder(args)
        .Build()
        .Run()
    0
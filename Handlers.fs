module CodeChallenge.Handlers

open Types
open System
open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Http
open Giraffe.SerilogExtensions
open Serilog


type NameResponse = { Name: string; Email: string }

type MoveResponse = { Move: Move }

let moveHandler: HttpHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {

            let logger = ctx.Logger()

            let serialize =
                ctx.GetJsonSerializer().SerializeToString

            let deserialize (text: string) =
                ctx.GetJsonSerializer().Deserialize<State>(text)

            let! current = ctx.BindJsonAsync<State>()

            let previous =
                current.Game.Id.ToString()
                |> State.load deserialize

            let state =
                match previous with
                | Some prev -> current |> State.mergeWith prev
                | None -> current

            state |> State.save serialize

            let move = Agent.getMove state logger

            logger.Information("Move : {@Move}", move)

            let response: MoveResponse = { Move = move }

            return! json response next ctx

        }

let nameHandler: HttpHandler =
    json
        { Name = "Evilz"
          Email = "vbourdon@veepee.com" }

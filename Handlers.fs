module Handlers
open Types
open System
open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Http



type MoveResponse = {
    Move : Move
}

let moveHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {

            let! current = ctx.BindJsonAsync<State>()

            let serialize = ctx.GetJsonSerializer().SerializeToString
            let deserialize (text:string) = ctx.GetJsonSerializer().Deserialize<State>(text)

            let previous = current.Game.Id.ToString() |> State.load deserialize

            let state = match previous with
                        | Some prev -> current |> State.mergeWith prev 
                        | none -> current

            state |> State.printMatrix

            state |> State.save serialize
            // TODO: use Ilogger 
            //printfn "%A" request
            let move = Agent.getMove state
            printfn "Move : %A" move
            let response :MoveResponse = { Move = move}
            return! json response next ctx
          
        }

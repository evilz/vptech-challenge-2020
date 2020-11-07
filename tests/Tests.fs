module Tests.Integration

open System.Net.Http
open CodeChallenge
open CodeChallenge.Types
open Microsoft.AspNetCore.Mvc.Testing
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Text.Json
open System.Text.Json.Serialization
open Xunit
open FsUnit.Xunit

let options =
    JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

options.Converters.Add
    (JsonFSharpConverter
        (unionEncoding = JsonUnionEncoding.UnwrapFieldlessTags, unionTagNamingPolicy = JsonNamingPolicy.CamelCase))


let defaultState =
    """ {
      "game": { "id": "3e01f847-0501-4141-9772-14b58f5b2d18" },
      "player": {
        "id": "3e01f847-0501-4141-9772-14b58f5b2d18",
        "name": "Evilz",
        "position": { "y": 3, "x": 3 },
        "previous": { "y": 3, "x": 3 },
        "area": { "y1": 0, "x1": 0, "y2": 4, "x2": 4 },
        "fire": true
      },
      "board": {
        "size": { "height": 5, "width": 5 },
        "walls": [
          { "y": 0, "x": 0 },
          { "y": 0, "x": 1 },
          { "y": 0, "x": 2 },
          { "y": 0, "x": 3 },
          { "y": 0, "x": 4 },

          { "y": 4, "x": 0 },
          { "y": 4, "x": 1 },
          { "y": 4, "x": 2 },
          { "y": 4, "x": 3 },
          { "y": 4, "x": 4 },

          { "y": 0, "x": 0 },
          { "y": 1, "x": 0 },
          { "y": 2, "x": 0 },
          { "y": 3, "x": 0 },
          { "y": 4, "x": 0 },

          { "y": 0, "x": 4 },
          { "y": 1, "x": 4 },
          { "y": 2, "x": 4 },
          { "y": 3, "x": 4 },
          { "y": 4, "x": 4 }
        ]
      },
      "players": [],
      "enemies": [
        { "y": 1, "x": 1, "neutral": true }
      ]
    }"""
    |> fun x -> JsonSerializer.Deserialize<Types.State>(x, options)

type IntegrationTests(factory: WebApplicationFactory<Program.Startup>) =

    do System.IO.File.Delete(sprintf "%s.json" defaultState.Game.Id)

    [<Fact>]
    let ``GET /name should say my name`` () =
        task {

            let client = factory.CreateClient()

            let! response = client.GetAsync("/name")

            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.NameResponse>(json, options)

            data.Name |> should equal "Evilz"
            data.Email |> should equal "vbourdon@veepee.com"


        }



    [<Fact>]
    let ``Should go to neutral`` () =
        task {
            let client = factory.CreateClient()

            let requestBody =
                JsonSerializer.Serialize(defaultState, options)

            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should
                be
                   (subsetOf [ Types.Move.Up
                               Types.Move.Left ])


        }


    [<Fact>]
    let ``Should shoot neutral`` () =
        task {
            let client = factory.CreateClient()

            let state =
                { defaultState with
                      Player =
                          { defaultState.Player with
                                Position = { Y = 1; X = 3 } } }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.``Fire-left`` ])
        }

    [<Fact>]
    let ``Should not shoot neutral cause of wall`` () =
        task {
            let client = factory.CreateClient()

            let walls =
                { Y = 1; X = 2 } :: defaultState.Board.Walls

            let state =
                { defaultState with
                      Player =
                          { defaultState.Player with
                                Position = { Y = 1; X = 3 } }
                      Board =
                          { defaultState.Board with
                                Walls = walls } }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.Down ])
        }


    [<Fact>]
    let ``Should shoot agressif first`` () =
        task {
            let client = factory.CreateClient()

            let enemies =
                { Y = 3; X = 3; Neutral = false }
                :: defaultState.Enemies

            let state =
                { defaultState with
                      Player =
                          { defaultState.Player with
                                Position = { Y = 1; X = 3 } }
                      Enemies = enemies }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.``Fire-down`` ])
        }

    [<Fact>]
    let ``Should get neutral before shooting`` () =
        task {
            let client = factory.CreateClient()

            let enemies =
                { Y = 3; X = 2; Neutral = false }
                :: defaultState.Enemies

            let state =
                { defaultState with
                      Player =
                          { defaultState.Player with
                                Position = { Y = 1; X = 2 } }
                      Enemies = enemies }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.Left ])
        }

    [<Fact>]
    let ``Should get neutral before shooting again`` () =
        task {
            let client = factory.CreateClient()

            let enemies =
                [ { Y = 3; X = 1; Neutral = false }
                  { Y = 1; X = 1; Neutral = true } ]

            let state =
                { defaultState with
                      Player =
                          { defaultState.Player with
                                Position = { Y = 1; X = 2 } }
                      Enemies = enemies }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.Left ])
        }

    [<Fact>]
    let ``Should get get away from enemy if cannot shoot`` () =
        task {
            let client = factory.CreateClient()
            let enemies = [ { Y = 3; X = 2; Neutral = false } ]

            let state =
                { defaultState with
                      Enemies = enemies
                      Player =
                          { defaultState.Player with
                                Fire = false } }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.Up ])
        }

    [<Fact>]
    let ``Should get get away from other player`` () =
        task {
            let client = factory.CreateClient()
            let enemies = []

            let state =
                { defaultState with
                      Enemies = enemies
                      Player =
                          { defaultState.Player with
                                Fire = false }
                      Players = [ { Y = 3; X = 2 } ] }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.Up ])
        }

    [<Fact>]
    let ``Should get get away from other player even when can fire`` () =
        task {
            let client = factory.CreateClient()
            let enemies = []

            let state =
                { defaultState with
                      Enemies = enemies
                      Player = { defaultState.Player with Fire = true }
                      Players = [ { Y = 3; X = 2 } ] }

            let requestBody = JsonSerializer.Serialize(state, options)
            let! response = client.PostAsync("/move", new StringContent(requestBody))
            response.EnsureSuccessStatusCode() |> ignore

            let! json = response.Content.ReadAsStringAsync()

            let data =
                JsonSerializer.Deserialize<Handlers.MoveResponse>(json, options)

            [ data.Move ]
            |> should be (subsetOf [ Types.Move.Up ])
        }


    interface IClassFixture<WebApplicationFactory<Program.Startup>>

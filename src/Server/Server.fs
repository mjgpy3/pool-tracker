module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open System.IO

open Shared

let gameApi =
    { saveGame = fun game ->
        File.WriteAllTextAsync(sprintf "/home/michael/dev/scratch/safe/%s.fs" (game.FinishedAt.ToString().Replace(":","-").Replace("/", "-")), sprintf "%A" game) |> Async.AwaitTask
    }

let webApp =
  ()
    |> Remoting.createApi
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue gameApi |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app

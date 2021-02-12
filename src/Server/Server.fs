module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open Game

let gameApi = 
    let asyncStartGame = fun (i:int) -> async {return Game.startGame i}
    let asyncIsValidGuess = fun (s:string, g:Game) -> async {return Game.isValidGuess s g}
    let asyncScore = fun (s:string, g:Game) -> async {return Game.score s g}
    { StartGame = asyncStartGame
      IsValidGuess = asyncIsValidGuess
      Score = asyncScore }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue gameApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app

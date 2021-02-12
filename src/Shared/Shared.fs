namespace Shared

open System

type Game = 
    {
        CodeLen : int
        SecretCode : string
        History : list<Guess>
    }
and Guess = 
    {
        Guess : string
        ScorePlace : int Option
        ScoreOther : int Option
    }

type IGameApi = 
    { StartGame : int -> Async<Game> //Async<Game * string>
      IsValidGuess : string * Game -> Async<bool> 
      Score : string * Game -> Async<Game> }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName


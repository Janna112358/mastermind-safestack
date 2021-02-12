module Game

open System

open Shared
open SecretCode


let startGame inputLen = 
    let codeLen, message = 
        if inputLen > 10 then
            10, "Maximum code length is 10"
        else if inputLen < 1 then
            1, "Minimum code length is 1"
        else 
            inputLen, "Code length accepted"
    
    let code = randomCode codeLen 
    { CodeLen = codeLen; SecretCode = code; History = [] }//, message

let isValidGuess ( guess : string ) (game : Game ) = 
    let isNumber = 
        guess 
        |> Seq.forall Char.IsDigit 
    let lengthOk = 
        String.length guess = game.CodeLen 
    isNumber && lengthOk

// let newGuess (guess : string) ( game : Game ) = 
//     let newGuess = 
//         { Guess = guess; ScorePlace = None; ScoreOther = None }
//     { game with History = game.History @ [newGuess] }

let score (guess : string) (game : Game ) = 
    let lCode = SecretCode.toList game.SecretCode
    let lGuess = SecretCode.toList guess

    let mutable countCorrect = 0
    let mutable countIn = 0

    let digits = [0 .. 9]
    for i in digits do
        let inCode = 
            lCode
            |> List.filter (fun e -> e = i)
            |> List.length
        let inGuess = 
            lGuess
            |> List.filter (fun e -> e = i)
            |> List.length 
        
        let inBoth = List.min [inCode; inGuess]
        countIn <- countIn + inBoth

    for i in [0 .. (game.CodeLen - 1)] do
        if lCode.[i] = lGuess.[i] then
            countCorrect <- countCorrect + 1
            countIn <- countIn - 1
    
    let newGuess = 
        { Guess = guess; ScorePlace = Some countCorrect; ScoreOther = Some countIn}
    { game with History = game.History @ [newGuess] }
    

    

namespace Mastermind

open System

module Code = 

    let toString (code : list<int>) = 
        code
        |> List.map string
        |> String.Concat

    let toList (code : string) = 
        [
            for c in code ->
                Char.GetNumericValue c |> int
        ]
        
    let randomCode (codeLen : int) = 
        let rd = Random()
        let listCode = [ for i in 1 .. codeLen -> rd.Next(0, 10)]
        listCode |> toString
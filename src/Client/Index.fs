module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model =
    { Game : Game Option 
      CodeLen : int 
      ValidGuess : bool
      NewGuess : string Option
    }

type Msg =
    | StartGame
    | UpdateGame of Game
    | InputGuess of string
    | IsValidInputGuess of bool
    | ScoreGuess

let gameApi = 
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IGameApi>

let init(): Model * Cmd<Msg> =
    let model =
        { Game = None 
          CodeLen = 4 
          ValidGuess = false 
          NewGuess = None }
    model, Cmd.none

let update msg model = 
    match model, msg with
    | model, StartGame ->
        match model.Game with 
        | None -> model, Cmd.OfAsync.perform gameApi.StartGame model.CodeLen UpdateGame
        | Some _ -> model, Cmd.none
    | model, UpdateGame newGame ->
        {model with Game = Some newGame}, Cmd.none
    | model, InputGuess guess ->
        match model.Game with 
        | None -> model, Cmd.none // to do: error (this should not happen)
        | Some game -> {model with NewGuess = Some guess}, Cmd.OfAsync.perform gameApi.IsValidGuess (guess, game) IsValidInputGuess
    | model, IsValidInputGuess isValid ->
        match isValid with
        | false -> {model with ValidGuess = false}, Cmd.none
        | true -> {model with ValidGuess = true}, Cmd.none
    | model, ScoreGuess ->
        match model.Game with
        | None -> model, Cmd.none //to do: error (this should not happen)
        | Some game -> 
            match model.NewGuess with 
            | None -> model, Cmd.none // to do: probably should not happen?
            | Some newGuess -> {model with NewGuess = None}, Cmd.OfAsync.perform gameApi.Score (newGuess, game) UpdateGame
    | _, _ -> 
        model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma


let viewListAsTiles stringList = 
    stringList
    |> List.map (fun g -> 
        Tile.child [] [
            str g
        ]
        )

let viewNewGuess ( model : Model ) ( dispatch : Msg -> unit ) = 
    match model.Game with 
    | None -> []
    | Some _ -> 

        [ Tile.child 
            []
            [
                Field.div [ Field.IsGrouped ]
                    [
                        Label.label 
                            [ 
                                Label.Modifiers [ Modifier.Spacing (Spacing.MarginLeftAndRight, Spacing.Is2) ]
                            ] 
                            [ str "Next Guess" ]
                        Control.div 
                            [
                                
                            ] 
                            [
                                Input.text
                                    [ 
                                    Input.Color IsInfo
                                    Input.Size IsSmall 

                                    Input.Placeholder "1234"
                                    Input.OnChange (fun x -> dispatch (InputGuess x.Value) )
                                    ]
                            ]

                        Button.button 
                            [
                                Button.Modifiers [ Modifier.Spacing (Spacing.MarginLeftAndRight, Spacing.Is2) ]
                                Button.Color IsInfo
                                Button.Disabled (not model.ValidGuess)
                                Button.OnClick (fun _ -> dispatch ScoreGuess )
                            ] 
                            [str "Submit"]

                        Help.help 
                            [
                                Help.Color IsDanger
                            ]
                            [ match model.ValidGuess with 
                                | true -> str ""
                                | false -> str "Invalid guess"
                            ]
                    ]
            ] 
        ]

let viewGuessHistory model dispatch = 
    match model.Game with
    | Some game ->
        game.History 
        |> List.map (fun guess -> 
            
            let displayStr = 
                match guess.ScorePlace, guess.ScoreOther with 
                | Some a, Some b ->
                    sprintf "guess: %s    %d %d" guess.Guess a b
                | _, _ -> 
                    sprintf "guess: %s    - -" guess.Guess 

            Tile.child [] [str displayStr] 
            )
    | None -> []

let view (model : Model) (dispatch : Msg -> unit) =
    Tile.ancestor [
        Tile.IsVertical
        Tile.Modifiers [ Modifier.Spacing (Spacing.MarginLeftAndRight, Spacing.Is2) ]
    ] [
        Tile.parent [
        ] [
            Tile.child [
                Tile.Size Tile.Is8
            ] [
                Heading.h1 []
                    [ str "Welcome to Mastermind" ]
            ]
        ]

        Tile.parent [ 
        ] [
            Tile.child [
                Tile.Size Tile.Is3
                Tile.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) 
                                 Modifier.Spacing (Spacing.PaddingTop, Spacing.Is6) ]
            ] [
                (Button.button [
                    Button.Color IsPrimary
                    Button.OnClick (fun _ -> dispatch StartGame)
                ]
                [ str "Start" ])
            ]

            Tile.child [
                Tile.Size Tile.Is5
            ] [
                Box.box' []
                    [ (Heading.h4 []
                        [str "rules" ])
                      (str "This is a code breaking game. Crack the code made up of digits 0-9 by putting in consecutive guesses. 
                      After each guess, you will get two scores: The first score indicates the number of digits that are correct AND
                      in the right place. The second indiciates the number of other correct digits, i.e. which are NOT in the right place.
                      Try and use as few guesses as possible. ")
                      (str "Press \"start\" to begin, good luck!")
                    ]
            ]
        ]

        Tile.parent [
            Tile.Modifiers [ Modifier.Spacing (Spacing.MarginLeftAndRight, Spacing.Is6) ]
        ] 
            ( match model.Game with 
            | Some game ->
                [
                    str ("Secret code: " + game.SecretCode)
                ]
            | None-> [str "..."] )

        Tile.parent 
            [
                Tile.IsVertical
                Tile.Modifiers [ Modifier.Spacing (Spacing.MarginLeftAndRight, Spacing.Is6) ]
            ] 
            ( viewGuessHistory model dispatch )
        
        Tile.parent 
            [
                Tile.Modifiers [ Modifier.Spacing (Spacing.MarginLeftAndRight, Spacing.Is6) ]
            ]
            (viewNewGuess model dispatch )
    ]
    
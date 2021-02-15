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
    | CheckWinner
    | ClearGame

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
    | model, ClearGame -> 
        init()
    | _, _ -> 
        model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let viewSecret model distpach = 
    let secretString =
        match model.Game with 
        | Some game -> 
            sprintf "Secret code: %s" game.SecretCode 
        | None -> "..."
    [
        Tile.child 
            [Tile.Modifiers [Modifier.TextColor IsGreyLight]] 
            [str secretString]
    ]

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
    | Some game -> 

        match game.IsWinner with 
        | true -> 
            [ Tile.child 
                [
                    Tile.Modifiers [ 
                    Modifier.TextAlignment (Screen.All, TextAlignment.Centered) 
                    Modifier.TextColor IsSuccess
                    Modifier.TextSize (Screen.All, TextSize.Is4)
                ] ]
                [str "~~~~Winner!~~~~"]
            ]
        | false -> 

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
                    sprintf "%s    --> %d - %d" guess.Guess a b
                | _, _ -> 
                    sprintf "%s    --> ... " guess.Guess 

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
            Tile.parent 
                [
                    Tile.Size Tile.Is1
                    Tile.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
                    Tile.IsVertical
                ] 
                [
                        Tile.child [ ] [
                        Button.button 
                            [
                                Button.Color IsPrimary
                                Button.Disabled ( not model.Game.IsNone )
                                Button.OnClick (fun _ -> dispatch StartGame)
                            ]
                            [ str "Start" ]
                        ]

                        Tile.child [] [
                        Button.button 
                            [
                                Button.Color IsDanger
                                Button.Disabled ( model.Game.IsNone )
                                Button.OnClick (fun _ -> dispatch ClearGame)
                            ]
                            [ str "Clear"]
                        ]
                ]

            Tile.child [
                Tile.Size Tile.Is5
            ] [
                Box.box' []
                    [ (Heading.h3 []
                        [str "rules" ])
                      (str "This is a code breaking game. Crack the code of four digits (0-9) by putting in consecutive guesses. 
                      After each guess, you will get two scores: The first score indicates the number of digits that are correct AND
                      in the right place. The second indiciates the number of other correct digits, i.e. which are NOT in the right place.
                      Try and use as few guesses as possible...")
                      (Heading.h5 [] 
                        [str "Press \"start\" to begin, good luck!"])
                    ]
            ]
        ]

        Tile.parent 
            [
                Tile.Size Tile.Is8
                Tile.IsVertical
                Tile.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
                Tile.Modifiers [ Modifier.BackgroundColor IsDark
                                 Modifier.TextColor IsWhite ]
                //Tile.Modifiers [ Modifier.Spacing ( Spacing.MarginLeft, Spacing.Is6) ]
            ] 
            //( (viewSecret model dispatch ) @ ( viewGuessHistory model dispatch ) )
            ( viewGuessHistory model dispatch )
        
        Tile.parent 
            [
                Tile.Size Tile.Is8
                Tile.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered)
                                 //Modifier.Spacing (Spacing.MarginLeftAndRight, Spacing.Is6)
                               ]
            ]
            (viewNewGuess model dispatch )
    ]
    
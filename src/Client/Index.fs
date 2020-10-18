module Index

open Elmish
open Fable.Remoting.Client
open Shared

let pockets =
  [
    UpperLeft
    UpperRight
    CenterLeft
    CenterRight
    LowerLeft
    LowerRight
  ]

type OneOrTwo =
  | One
  | Two

type Ball =
  | CueBall
  | NumberedBall of int

type Model =
    { Player1Name: string
      Player2Name: string
      Turn: OneOrTwo
      BallsOnTable: int Set
      TurnEvents: Turn
      SelectedBall: Ball Option
      Game: Turn list
      InEndGame: bool }

let modelToGame (wasCorrect8: bool) (model: Model): Game =
  { Player1Name = model.Player1Name
    Player2Name = model.Player2Name
    WasCorrect8Called = wasCorrect8
    Game = model.Game
    FinishedAt = System.DateTime() }

type Msg =
    | SetPlayerName of OneOrTwo*string
    | SelectBall of Ball
    | SelectPocket of Pocket
    | EndTurn
    | EndGame
    | Correct8 of bool
    | SavedGame of unit

let gameApi =
  ()
    |> Remoting.createApi
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IGameApi>

let init(): Model * Cmd<Msg> =
    let model =
        { Player1Name = ""
          Player2Name = ""
          Turn = One
          BallsOnTable = Set.ofList [1..15]
          TurnEvents = []
          SelectedBall = None
          Game = []
          InEndGame = false }
    model, Cmd.none

let switchTurn = function
  | One -> Two
  | Two -> One

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg, model with
    | SetPlayerName (One, value), _ ->
        { model with Player1Name = value }, Cmd.none
    | SetPlayerName (Two, value), _ ->
        { model with Player2Name = value }, Cmd.none
    | SelectBall ball, _ ->
        { model with SelectedBall = Some ball }, Cmd.none
    | SelectPocket pocket, { SelectedBall=Some (NumberedBall ball) } ->
        { model with SelectedBall = None; TurnEvents=model.TurnEvents @ [NumberedBallSunk (ball, pocket)]; BallsOnTable=Set.remove ball model.BallsOnTable }, Cmd.none
    | SelectPocket pocket, { SelectedBall=Some CueBall } ->
        { model with SelectedBall = None; TurnEvents=model.TurnEvents @ [Scratched] }, Cmd.none
    | SelectPocket pocket, { SelectedBall=None } ->
        model, Cmd.none
    | EndTurn, { Turn=currentTurn } ->
        { model with Turn=switchTurn currentTurn; Game=model.Game @ [model.TurnEvents]; TurnEvents=[] }, Cmd.none
    | EndGame, { TurnEvents = [] } ->
        { model with InEndGame = true }, Cmd.none
    | EndGame, { TurnEvents = turnEvents } ->
        { model with Game=model.Game @ [turnEvents]; InEndGame = true }, Cmd.none
    | Correct8 wasCorrect8, _ ->
        model, Cmd.OfAsync.perform gameApi.saveGame (modelToGame wasCorrect8 model) SavedGame
    | SavedGame, _ ->
      init ()

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let playerName (model : Model) =
  match model.Turn with
    | One -> model.Player1Name
    | Two -> model.Player2Name

let containerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Field.div [ Field.IsGrouped ] [
            Control.p [ Control.IsExpanded ] [
                Input.text [
                  Input.Value model.Player1Name
                  Input.Placeholder "Player 1"
                  Input.OnChange (fun x -> SetPlayerName (One, x.Value) |> dispatch) ]
            ]
            Control.p [ Control.IsExpanded ] [
                Input.text [
                  Input.Value model.Player2Name
                  Input.Placeholder "Player 2"
                  Input.OnChange (fun x -> SetPlayerName (Two, x.Value) |> dispatch) ]
            ]
        ]
        Content.content [ ] [
          str ("Current turn: " + playerName model)
        ]
        Content.content [ ] [
          str "Balls"
        ]
        Field.div [ Field.IsGrouped ] (
        Control.p [ ] [
                Button.a [
                    Button.Color (if model.SelectedBall = Some CueBall then IsSuccess else IsPrimary)
                    Button.OnClick (fun _ -> SelectBall CueBall |> dispatch)
                ] [
                    str "Cue"
                ]
            ]::[
         for ball in Seq.toList model.BallsOnTable do
           Control.p [ ] [
                Button.a [
                    Button.Color (if model.SelectedBall = Some (NumberedBall ball) then IsSuccess else IsPrimary)
                    Button.OnClick (fun _ -> SelectBall (NumberedBall ball) |> dispatch)
                ] [
                    str <| string ball
                ]
            ]
        ])
        Content.content [ ] [
          str "Pockets"
        ]
        Field.div [ Field.IsGrouped ] [
         for pocket in pockets do
           Control.p [ ] [
                Button.a [
                    Button.Color IsPrimary
                    Button.OnClick (fun _ -> SelectPocket pocket |> dispatch)
                ] [
                    str <| string pocket
                ]
            ]
        ]
        Field.div [ Field.IsGrouped ] [
          Control.p [ ] [
              Button.a [
                  Button.Color IsPrimary
                  Button.OnClick (fun _ -> dispatch EndTurn)
              ] [
                  str "End Turn"
              ]
          ]
        ]
        Field.div [ Field.IsGrouped ] [
          Control.p [ ] [
              Button.a [
                  Button.Color IsWarning
                  Button.OnClick (fun _ -> dispatch EndGame)
              ] [
                  str "End Game"
              ]
          ]
        ]
    ]

let endGameBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Content.content [ ] [
          str "Was correct 8 ball called?"
        ]
        Field.div [ Field.IsGrouped ] [
          Control.p [ ] [
              Button.a [
                  Button.Color IsPrimary
                  Button.OnClick (fun _ -> Correct8 true |> dispatch)
              ] [
                  str "Yes"
              ]
          ]
          Control.p [ ] [
              Button.a [
                  Button.Color IsDanger
                  Button.OnClick (fun _ -> Correct8 false |> dispatch)
              ] [
                  str "No"
              ]
          ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is10)
                    Column.Offset (Screen.All, Column.Is1)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "pool_tracker" ]
                    if model.InEndGame then endGameBox model dispatch else containerBox model dispatch
                ]
            ]
        ]
    ]

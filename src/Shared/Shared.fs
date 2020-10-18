namespace Shared

open System

type Pocket =
  | UpperLeft
  | UpperRight
  | CenterLeft
  | CenterRight
  | LowerLeft
  | LowerRight

type GameEvent =
  | NumberedBallSunk of int*Pocket
  | Scratched

type Turn = GameEvent list

type Game =
    { Player1Name: string
      Player2Name: string
      WasCorrect8Called: bool
      Game: Turn list
      FinishedAt: DateTime }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IGameApi =
    { saveGame : Game -> Async<unit> }
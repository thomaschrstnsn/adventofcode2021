[<Struct>]
type Player = { Score : int; Position : int }

type Die = int seq

[<Struct>]
type Game = { Current : Player; Next : Player; Dice : Die ; Rolls : int}

let oneToHundred : Die = Seq.initInfinite (fun i -> (i % 100) + 1)  

let mkPlayer x = {Score = 0; Position = (x-1)}

let testInput = [mkPlayer 4; mkPlayer 8]
let input = [mkPlayer 7; mkPlayer 6]

let rec playGame (game : Game) : Game =
    let complete = [game.Current ; game.Next] |> Seq.map (fun p -> p.Score) |> Seq.exists (fun s -> s >= 1000)
    if complete
    then
        game
    else
        let roll = Seq.take 3 game.Dice |> Seq.sum
        let die' = Seq.skip 3 game.Dice
        let pos' = (game.Current.Position + roll) % 10
        let sco' = game.Current.Score + (pos' + 1)
        let p'   = {Position = pos'; Score = sco'}
        let rol' = game.Rolls + 3
        playGame {Current = game.Next; Next = p'; Dice = die'; Rolls = rol'}

let solvePt1 p1 p2 =
    let final = playGame {Current = p1; Next = p2; Dice = oneToHundred; Rolls = 0}
    let loser = [final.Next; final.Current] |> Seq.minBy (fun p -> p.Score)
    final.Rolls * loser.Score
    
solvePt1 testInput[0] testInput[1] |> printfn "test pt1: %d"
    
solvePt1 input[0] input[1] |> printfn "result pt1: %d"

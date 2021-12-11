let testInput = [| 
    "5483143223" ; 
    "2745854711" ; 
    "5264556173" ; 
    "6141336146" ; 
    "6357385478" ; 
    "4167524645" ;
    "2176841721" ;
    "6882881134" ;
    "4846848554" ;
    "5283751526" |]

let input = System.IO.File.ReadAllLines "input.txt"

let stringToInts (s:string) = s.ToCharArray () |> Array.map (fun c -> new string [| c |] |> int )

let parse (x : string[]) : int[,] = 
    let dim1 = x.Length
    let dim2 = x[0].Length
    let arrarr = x |> Array.map stringToInts 
    Array2D.init dim1 dim2 (fun i j -> arrarr[i][j])

let increaseAll (arr : int[,]) : int[,]= Array2D.map (fun x -> x + 1) arr
let resetBlinks (arr : int[,]) : int[,]= Array2D.map (fun x -> if x > 9 then 0 else x) arr

let increaseNeighbours (arr : int[,]) (coord : int * int) =
    let (i,j) = coord
    let neighbours = seq { for di in -1 .. 1 do 
                           for dj in -1 .. 1 do (di + i, dj + j)} |> Seq.filter (fun p -> p <> (i,j)) |> Set.ofSeq
    Array2D.mapi (fun i' j' el -> if Set.contains (i',j') neighbours then el + 1 else el ) arr

let getAllElements (a:'a[,]) : seq<int * int * 'a> =
    seq { for i in 0 .. a.GetLength(0)-1 do
          for j in 0 .. a.GetLength(1)-1 do yield (i, j, a.[i,j]) }

let getBlinking (energyLevels : int[,]) =  
    getAllElements energyLevels |> 
    Seq.filter (fun (_,_,el) -> el > 9) |> 
    Seq.map (fun (i,j,_) -> (i,j))

let getAllValues (a:'a[,]) : seq<'a> =
    seq { for i in 0 .. a.GetLength(0)-1 do
          for j in 0 .. a.GetLength(1)-1 do yield a.[i,j] }

let rec handleBlinks (energyLevels : int[,]) (alreadyHandled : Set<int*int>) = 
    let blinking = getBlinking energyLevels |> Set.ofSeq
    let unhandled = Set.difference blinking alreadyHandled

    if Set.isEmpty unhandled
    then energyLevels
    else
        let blink = Set.minElement unhandled
        let nextEnergy = increaseNeighbours energyLevels blink
        handleBlinks nextEnergy (alreadyHandled |> Set.union (Set.singleton blink))


let runStep (energyLevels : int[,]) =
    let inc = increaseAll energyLevels
    let afterBlinking = handleBlinks inc Set.empty
    (getBlinking afterBlinking |> Seq.length, resetBlinks afterBlinking)

let rec runSteps n els count = 
    match n with
    | 0 -> (count, els)
    | n -> 
        let (blinks, nextEnergy) = runStep els
        runSteps (n-1) nextEnergy (count + blinks)

runSteps 100 (parse testInput) 0 |> fst  |> printfn "part1 test %d"
runSteps 100 (parse input) 0 |> fst  |> printfn "part1 %d"

let rec solvePt2 els count =
    let (_, els') = runStep els
    let values = getAllValues els' |> Seq.distinct |> Set.ofSeq 
    if values = Set.singleton 0
    then
        count + 1
    else solvePt2 els' (count + 1)

solvePt2 (parse testInput) 0 |>  printfn "part2 test %d"
solvePt2 (parse input) 0 |>  printfn "part2 %d"

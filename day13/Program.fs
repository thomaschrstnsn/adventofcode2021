type Fold = Horizontal of int | Vertical of int
type Coord = int * int

let parseCoord (s:string) : Coord =
    let s = s.Split ','
    (int s[0], int s[1])
 
let parseFold (s:string) : Fold =
    match s.Split '=' with
    | [| "fold along x" ; v |] -> Horizontal <| int v
    | [| "fold along y" ; v |] -> Vertical <| int v
    | _ -> failwithf $"failed to parse {s} as a fold"

type Paper = {Coords: Coord Set ; Folds: Fold array}

let parse (lines : string array) : Paper =
    let notEmpty s = s <> ""
    let (coords, folds) = (Seq.takeWhile notEmpty lines, Seq.skipWhile notEmpty lines |> Seq.skip 1)
    {Coords = coords |> Seq.map parseCoord |> Set.ofSeq ; Folds = folds |> Seq.map parseFold |> Array.ofSeq }


let testInput = [| 
    "6,10" ;
    "0,14" ;
    "9,10" ;
    "0,3" ;
    "10,4" ;
    "4,11" ;
    "6,0" ;
    "6,12" ;
    "4,1" ;
    "0,13" ;
    "10,12" ;
    "3,4" ;
    "3,0" ;
    "8,4" ;
    "1,10" ;
    "2,14" ;
    "8,10" ;
    "9,0" ;
    "" ;
    "fold along y=7" ;
    "fold along x=5"
 |]

parse testInput |> printfn "%A"

let fold (coords : Coord Set) =
    function
    | Vertical n ->
        let untouched = Set.filter (fun (x,y) -> y <= n) coords
        let toBeChanged = Set.difference coords untouched
        let changed = toBeChanged |> Set.map (fun (x,y) -> (x, n-(y-n)))
        Set.union untouched changed
    | Horizontal n ->
        let untouched = Set.filter (fun (x,y) -> x <= n) coords
        let toBeChanged = Set.difference coords untouched
        let changed = toBeChanged |> Set.map (fun (x,y) -> (n-(x-n), y))
        Set.union untouched changed

let f1 = fold ((parse testInput).Coords) (Vertical 7)
f1 |> Set.count |> printfn "%d"

let f2 = fold f1 (Horizontal 5)
f2 |> Set.count |> printfn "%d"


let input = System.IO.File.ReadAllLines "input.txt"
let inp = parse input
let s1 = fold inp.Coords inp.Folds[0]
s1 |> Set.count |> printfn "pt1: %d"

let s2 = Array.fold fold inp.Coords inp.Folds
s2 |> Set.count |> printfn "pt2: %d"

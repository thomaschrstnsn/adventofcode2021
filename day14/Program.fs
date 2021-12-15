let testInput = [| 
    "NNCB" ;
    "" ;
    "CH -> B" ;
    "HH -> N" ;
    "CB -> H" ;
    "NH -> C" ;
    "HB -> C" ;
    "HC -> B" ;
    "HN -> C" ;
    "NN -> C" ;
    "BH -> H" ;
    "NC -> B" ;
    "NB -> B" ;
    "BN -> B" ;
    "BB -> N" ;
    "BC -> B" ;
    "CC -> N" ;
    "CN -> C" |]

type Lookup = Map<(char*char), char>

let parse (input : string array) :  (char list * Lookup)=
    let parseRule (s : string) : ((char * char) * char) =
        let split = s.Split "->" |> Array.map (fun s -> s.Trim())
        let pair = split[0].ToCharArray()
        let result = split[1].ToCharArray()
        match (pair, result) with
        | ([| first; second |], [| c |]) -> ((first, second), c)
        | _ -> failwithf "could not parse rule (pair %A result %A)" pair result

    let start = input[0]
    if input[1] = ""
    then
        let m = input |> Array.skip 2 |> Seq.map parseRule |> Map.ofSeq
        (start.ToCharArray() |> List.ofArray, m)
    else
        failwith "failed to parse"

let runRound (lookup : Lookup) (s : char list) =
    let rec r' (inp : char list) (out : char list) =
        match inp with
        | (a :: (b :: rest)) ->
            let o' =
                match Map.tryFind (a,b) lookup with
                | Some c -> (c :: (a :: out))
                | None -> (a :: out)
            r' (b :: rest) o'
        | [ c ] -> List.rev (c :: out)
        | [] -> List.rev out
    r' s []

let rec runRounds (n : int) (lookup : Lookup) (s : char list) =
    if n = 0
    then
        s
    else
        let s' = runRound lookup s
        runRounds (n-1) lookup s'

let joinCharList (cl : char list) =
    let a = cl |> Array.ofList
    new string (a)

let (start, lookup) = parse testInput
start |> runRounds 1 lookup |> joinCharList |> printfn "%A" 
start |> runRounds 2 lookup |> joinCharList |> printfn "%A" 
start |> runRounds 3 lookup |> joinCharList |> printfn "%A" 
start |> runRounds 4 lookup |> joinCharList |> printfn "%A" 

let freq (xs : 'a list) : ('a * int) seq =
    xs |> Seq.groupBy id |> Seq.map (fun (k, vals) -> (k, Seq.length vals))

let solvePt1 (lookup : Lookup) (s : char list) =
    let endstate = s |> runRounds 10 lookup
    let freqs = freq endstate
    let max = Seq.maxBy snd freqs
    let min = Seq.minBy snd freqs
    (min, max, (snd max) - (snd min))
    
solvePt1 lookup start |> printfn "pt1 test: %A"

let input = System.IO.File.ReadAllLines "input.txt"

let (s,l) = parse input

solvePt1 l s |> printfn "pt1 res: %A"


let solvePt2 (lookup : Lookup) (s : char list) =
    let endstate = s |> runRounds 40 lookup
    let freqs = freq endstate
    let max = Seq.maxBy snd freqs
    let min = Seq.minBy snd freqs
    (min, max, (snd max) - (snd min))

solvePt2 lookup start |> printfn "pt2 test: %A"


type Node = string

type Graph = Map<Node,Node>

let parseLine (line:string) = 
    match line.Split '-' with
    | [| ns ; ne |] -> (ns,ne)
    | _ -> failwith (sprintf "failed to parse '%s'" line)

let parse (inps : string array) : Graph =  inps |> Array.map parseLine |> Map.ofArray

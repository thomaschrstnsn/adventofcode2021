open System
open System.Collections

type Bit = B0 | B1

let bits (s : string) : Bit seq =
    let hexToBits =
        function
        | '0' -> [B0;B0;B0;B0]
        | '1' -> [B0;B0;B0;B1]
        | '2' -> [B0;B0;B1;B0]
        | '3' -> [B0;B0;B1;B1]
        | '4' -> [B0;B1;B0;B0]
        | '5' -> [B0;B1;B0;B1]
        | '6' -> [B0;B1;B1;B0]
        | '7' -> [B0;B1;B1;B1]
        | '8' -> [B1;B0;B0;B0]
        | '9' -> [B1;B0;B0;B1]
        | 'A' -> [B1;B0;B1;B0]
        | 'B' -> [B1;B0;B1;B1]
        | 'C' -> [B1;B1;B0;B0]
        | 'D' -> [B1;B1;B0;B1]
        | 'E' -> [B1;B1;B1;B0]
        | 'F' -> [B1;B1;B1;B1]
        | c -> failwithf $"unexpected char: '%A{c}'" 
    s.ToCharArray() |> Seq.collect hexToBits

let bitArrayToInt64 (ba : BitArray) : int64 =
    if ba.Length > 31
    then 
        let res = Array.init 8 (fun _ -> 0uy)
        ba.CopyTo(res, 0)
        BitConverter.ToInt64 (res, 0)
    else
        let res = Array.singleton 0
        ba.CopyTo(res, 0)
        res[0]
    
let bitsToInt64 (bs : Bit seq) : int64 =
    let b2b = function
        | B0 -> false
        | B1 -> true
    let a = bs |> Seq.map b2b  |> Seq.rev  |> Array.ofSeq
    let ba = BitArray(a)
    bitArrayToInt64 ba

type Operation
    = Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo

type Packet = {Version : int; content : PacketContent}
 and PacketContent
     = LiteralValue of int64
     | Operator of Operation * Packet list
    
let rec parsePackets (bits : Bit seq) : (Packet * Bit seq) =
    let rec readLiteral (bits : Bit seq) (res : Bit seq seq) : (int64 * Bit seq) =
        let head = Seq.head bits
        let group = bits |> Seq.skip 1 |> Seq.take 4
        let r' = Seq.append res (Seq.singleton group)
        let b' = bits |> Seq.skip 5
        if head = B1
        then
            readLiteral b' r'
        else
            (Seq.collect id r' |> bitsToInt64, b')

    let rec readPacketsFromBits (bits : Bit list) (res : Packet list) : Packet list =
        match bits with
        | [] -> List.rev res
        | _ ->
            let (p, rest) = parsePackets bits
            readPacketsFromBits (List.ofSeq rest) (p :: res)

    let rec readNumberOfPackets (bits : Bit seq) (c : int) (res : Packet list) : (Packet list * Bit seq)=
        match c with
        | 0 -> (List.rev res, bits)
        | _ ->
            let (p, rest) = parsePackets bits
            readNumberOfPackets rest (c-1) (p :: res)

    let rec readOperator (bits : Bit seq) (res : Packet List) : (Packet list * Bit seq) =
        match Seq.head bits with
        | B1 ->
            let numberOfSubpackets = bits |> Seq.skip 1 |> Seq.take 11 |> bitsToInt64 |> int
            let (ps, rest) = readNumberOfPackets (bits |> Seq.skip 12) numberOfSubpackets []
            (ps, rest)
        | B0 ->
            let packetBitCount = bits |> Seq.skip 1 |> Seq.take 15 |> bitsToInt64 |> int
            let packetBits = bits |> Seq.skip 16 |> Seq.take packetBitCount |> List.ofSeq
            (readPacketsFromBits packetBits [], Seq.skip (16 + packetBitCount) bits)
    
    let version = bits |> Seq.take 3 |> bitsToInt64 |> int
    let typeId = bits |> Seq.skip 3 |> Seq.take 3 |> bitsToInt64 |> int
    let b' = bits |> Seq.skip 6
    let (pc, rest) =
        match typeId with
        | 4 ->
            let (p, rest) = readLiteral b' []
            (LiteralValue p, rest)
        | x ->
            let op = match x with
                     | 0 -> Sum
                     | 1 -> Product
                     | 2 -> Minimum
                     | 3 -> Maximum
                     | 5 -> GreaterThan
                     | 6 -> LessThan
                     | 7 -> EqualTo
                     | _ -> failwithf $"could not determine operation from %d{x}"
            let (p, rest) = readOperator b' []
            (Operator (op,p), rest)
    ({Version = version; content = pc}, rest)

let rec parsePackets_ (bits : Bit seq) : Packet =
    let (p, rest) = parsePackets bits
    if Seq.forall (fun x -> x = B0) rest
    then
        p
    else
        let _ = printfn "incomplete parse"
        p
    
"D2FE28" |> bits |> parsePackets_ |> printfn "%A"
"38006F45291200" |> bits |> parsePackets_ |> printfn "%A"
"EE00D40C823060"|> bits |> parsePackets_ |> printfn "%A"

let rec versionSum (p : Packet) : int =
    let subSum =
        match p.content with
        | Operator (_, ps) ->
            ps |> List.sumBy versionSum
        | LiteralValue lv -> 0
    p.Version + subSum

let parseAndSum = bits >> parsePackets_ >> versionSum
    
"8A004A801A8002F478" |> parseAndSum |> printfn "16 = %d"
"620080001611562C8802118E34" |> parseAndSum |> printfn "12 = %d"
"C0015000016115A2E0802F182340" |> parseAndSum |> printfn "23 = %d"
"A0016C880162017C3686B18A3D4780" |> parseAndSum |> printfn "31 = %d"

let input = System.IO.File.ReadAllText "input.txt"

let parsedInput = input |> bits |> parsePackets_

parsedInput |> versionSum |> printfn "pt 1: %d"

let rec evaluate (p : Packet) : int64 =
    match p.content with
    | LiteralValue v -> v
    | Operator(operation, packets) ->
        let input = packets |> List.map evaluate
        let twoOperands (twoOpF: Int64 -> Int64 -> Int64) : (Int64 list -> Int64) =
                (fun xs ->
                    let (a,b) =
                        match xs with
                        | [x;y] -> (x,y)
                        | _ -> failwithf $"expected two operands, but got %d{List.length xs}"
                    twoOpF a b)
        let (f : Int64 list -> Int64)  =
            match operation with
            | Sum -> List.sum
            | Product -> List.fold (fun s v -> s * v) 1
            | Maximum -> List.max
            | Minimum -> List.min
            | GreaterThan ->
                twoOperands (fun x y -> if x > y then 1 else 0)
            | LessThan ->
                twoOperands (fun x y -> if x < y then 1 else 0)
            | EqualTo ->
                twoOperands (fun x y -> if x = y then 1 else 0)
        f input
        
let parseAndEval = bits >> parsePackets_ >> evaluate

"C200B40A82" |> parseAndEval |> printfn "sum 3 = %d"
"04005AC33890" |> parseAndEval |> printfn "product 54 = %d"
"880086C3E88112" |> parseAndEval |> printfn "minimum 7 = %d"
"CE00C43D881120" |> parseAndEval |> printfn "maximum 9 = %d"
"D8005AC2A8F0" |> parseAndEval |> printfn "less than 1 = %d"
"F600BC2D8F" |> parseAndEval |> printfn "grater than 0 = %d"
"9C005AC2F8F0" |> parseAndEval |> printfn "equal to 0 = %d"
"9C0141080250320F1802104A08" |> parseAndEval |> printfn "complex 1 = %d"

parsedInput |> evaluate |> printfn "pt 2: %d"

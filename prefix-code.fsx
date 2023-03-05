open System

let n = int(Console.In.ReadLine())
let table = 
    [ for i in 0 .. n - 1 do
        let token = (Console.In.ReadLine()).Split [|' '|]
        let byte = token.[0]
        let asciicode = int(token.[1]) in asciicode,byte] |> List.sortByDescending (fun (c,b) -> String.length b)

let (|Prefix|_|) (input:string)  = 
    List.tryFind (fun ((c,b):int*string) -> input.StartsWith(b)) table

let rec solve dec index enc =
    let res = (|Prefix|_|) enc in 
    match enc with 
    | "" -> printfn "%s" dec
    | Prefix s -> let (ascii,byte) = res.Value in 
                    let d = char ascii |> string in 
                        Seq.skip (String.length byte) enc 
                        |> System.String.Concat 
                        |> solve (dec+d) (index+(String.length byte)) 
    | _ -> printfn "DECODE FAIL AT INDEX %i" index


let s = Console.In.ReadLine() in solve "" 0 s

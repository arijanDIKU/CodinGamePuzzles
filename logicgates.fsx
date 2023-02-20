open System

let n = int(Console.In.ReadLine())
let m = int(Console.In.ReadLine())

let inputs = 
    [ for i in 0 .. n - 1 do
        let token = (Console.In.ReadLine()).Split [|' '|]
        let inputName = token.[0]
        let inputSignal = token.[1] in inputName, inputSignal ] 

let outputs = 
    [ for i in 0 .. m - 1 do
        let token1 = (Console.In.ReadLine()).Split [|' '|]
        let outputName = token1.[0]
        let _type = token1.[1]
        let inputName1 = token1.[2]
        let inputName2 = token1.[3] in outputName, _type, inputName1, inputName2 ]

let solution () : unit = 
    let f op = List.map2 (fun a b -> op a b) 
    let transform (input:(string*string)) : bool list = snd input |> Seq.map (fun c -> match c with | '-' -> true | _ -> false) |> List.ofSeq
    outputs |> List.map (fun (oname,tp,in1,in2) -> let in1', in2' = List.find (fun (iname, isignal) -> iname = in1) inputs |> transform,
                                                                    List.find (fun (iname, isignal) -> iname = in2) inputs |> transform in oname, match tp with 
                                                                                                                                                    | "AND" -> f (&&) in1' in2'
                                                                                                                                                    | "OR" -> f (||) in1' in2'
                                                                                                                                                    | "XOR" -> f (<>) in1' in2'
                                                                                                                                                    | "NAND" -> f (&&) in1' in2' |> List.map (not)
                                                                                                                                                    | "NOR" -> f (||) in1' in2' |> List.map (not)
                                                                                                                                                    | "NXOR" -> f (<>) in1' in2' |> List.map (not) 
                                                                                                                                                    | _ -> failwith (nameof failwith) ) 
    |> List.iter (fun (oname, signalseq) -> printf "%s " oname; List.iter (function | true -> printf "-" | false -> printf "_") signalseq; printfn "")                                                                                                       

solution ()



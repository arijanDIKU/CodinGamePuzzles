open System


let cards = List.append ("A"::[for i in 2..9 do string i]) ["T"; "J"; "Q"; "K"] |> List.zip (List.replicate 13 4) 
let streamOfConsciousness = Console.In.ReadLine()
let bustThreshold = int(Console.In.ReadLine())

let parse (stream:string) : string = 
    stream.Split('.') |> Array.filter (fun s -> Seq.forall (fun c -> List.contains (4,(string c)) cards) s) |> String.concat ""

let evaluate (parsedStream: string) : unit = 
    let remaining =
        cards |> List.map (fun (cnt,card) -> 
                                                Seq.fold (fun sum obs -> if obs=(char card) then sum+1 else sum) 0 parsedStream 
                                                    |> (fun sum -> (cnt-sum,card) ) ) in 
                let totalrems = List.sumBy fst remaining |> float in let lessthan = List.take (bustThreshold-1) remaining in 
                                                                                        let lessthansum = List.sumBy fst lessthan |> float in 
                                                                                            (lessthansum/totalrems)*100. |> printfn "%1.0f%%"
        


parse streamOfConsciousness |> evaluate

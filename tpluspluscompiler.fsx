open System

let line = Console.In.ReadLine()


let (+-) controller (line:string) = 
    match line.[0] with 
    | '<' -> controller+1
    | '>' -> controller-1
    | _ -> failwith "unknown character"


let rec solution longestThusFar controller (line:string) = 
    let result = controller +- line
    match result, String.length line with
    | -1,_  -> longestThusFar
    | _,1  -> longestThusFar+1
    | n -> solution (longestThusFar+1) result line.[1..]


solution 0 0 line |> printfn "%i"

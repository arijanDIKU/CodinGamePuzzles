open System

let N = int(Console.In.ReadLine())
let lines =  
    [ for i in 0 .. N - 1 do
        let token = (Console.In.ReadLine()).Split [|' '|] in
        let name = token.[0] in 
        let r = int(token.[1]) in
        let c = int(token.[2]) in name,r,c] in 
    List.groupBy (fun (n,r,c) -> r) lines 
    |> List.sortBy fst
    |> List.mapi (fun I (i,line) -> if I%2=0 then List.sortBy (fun (n,r,c) -> c) line else List.sortByDescending (fun (n,r,c) -> c) line)
    |> List.fold (fun l line -> List.fold (fun s (n,_,_) -> s + ($",{n}") ) "" line |> (+) l) "" 
    |> Seq.tail |> System.String.Concat |> printfn "%s"


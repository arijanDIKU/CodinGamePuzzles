open System

let X = int(Console.In.ReadLine())
let N = int(Console.In.ReadLine())
let words = (Console.In.ReadLine()).Split [|' '|]

let formula w L = ((float L) * 6.5 / 100.) * 10. * w
let weights = [for i in 0 .. N - 1 do float(words.[i]) ] |> List.sortByDescending id |> List.chunkBySize X in 
    List.mapi (fun L row -> List.fold (fun wl weight -> wl + formula weight L) 0.0 row ) weights 
    |> List.sum |> printf "%.3f"



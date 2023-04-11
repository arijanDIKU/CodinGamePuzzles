open System

let line1 = Console.In.ReadLine() |> Seq.chunkBySize 3 |> Seq.map (fun n -> System.String.Concat n)
let line2 = Console.In.ReadLine() |> Seq.chunkBySize 3 |> Seq.map (fun n -> System.String.Concat n)
let line3 = Console.In.ReadLine() |> Seq.chunkBySize 3 |> Seq.map (fun n -> System.String.Concat n)



let solver (X:string) = 
    let num = Seq.countBy (fun c -> c<>' ') X |> Seq.last |> snd in  
        match num with 
        | 2 -> printf "1"
        | 3 -> printf "7"
        | 4 -> printf "4"
        | 5 -> match X[3] with 
               | '|' -> printf "5"
               | _   -> match X[6] with 
                        | '|' -> printf "2"
                        | _ ->   printf "3"

        | 6 -> match X[5] with 
               | ' ' -> printf "6"
               | _ -> match X[6] with 
                      | '|' -> printf "0"
                      | _ -> printf "9"
        | _ -> printf "8"



let map = Seq.map3 (fun t m b -> string t + string m + string b) line1 line2 line3 in
    Seq.iter solver map 





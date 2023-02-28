open System

let abbreviations = ["sp";"bS";"sQ";"nl"]
let T = Console.In.ReadLine() in 
    T.Split(' ') |> Array.map (fun chunk -> let digits = Seq.takeWhile (fun c -> Char.IsDigit c) chunk |> String.Concat in 
                                            let chars = Seq.skipWhile (fun c -> Char.IsDigit c) chunk |> String.Concat  in 
                                            match (digits,chars) with
                                            | (n,abbr) when List.contains abbr abbreviations -> let times = if n="" then 1 else int n in 
                                                                                                            (match abbr with 
                                                                                                            | "sp" -> " "
                                                                                                            | "bS" -> "\\"
                                                                                                            |  "sQ"-> "'"
                                                                                                            | "nl" -> "\n"
                                                                                                            | _ -> failwith (nameof failwith)) |> String.replicate times 

                                            | (n,"") -> let last = Seq.last n |> (fun c -> int c - int '0') |> string in 
                                                        let n' = Seq.rev n |> Seq.tail |> Seq.rev |> String.Concat |> int in 
                                                        String.replicate n' last 
                                            | (n,c) -> String.replicate (int n) c
                                            ) |> Array.iter (printf "%s") 

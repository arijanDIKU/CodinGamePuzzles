open System


 
let token = (Console.In.ReadLine()).Split [|' '|]
let W = int(token.[0])
let H = int(token.[1])
let toplabels = Console.In.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
let lines = [for i in 0..H-3 do Console.In.ReadLine()]
let botlabels = Console.In.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
let readLine (line:string) : int list = 
    line.Split('|', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (function | "  " -> 0 | "--" -> 1)
    |> List.ofArray

let parsedLines = List.map readLine lines 

    
let solution () : unit = 
    let initPositions = List.init (W/3+1) id in 
    parsedLines |> List.fold (fun currentPositions (line:int list) -> 
                                 List.map (fun p -> let last = List.length currentPositions-1 in 
                                                                        match p with 
                                                                        | 0 -> p+line.[0]
                                                                        | n when n=last -> p-line.[last-1]
                                                                        | n -> 
                                                                            if line.[n-1]=1 then 
                                                                                (n-1)
                                                                            elif line.[n]=1 then 
                                                                                (n+1)      
                                                                            else    
                                                                                p) currentPositions) initPositions 
    |> List.iteri (fun i p -> printfn "%s%s" toplabels.[i] botlabels.[p]) 
                                                
solution ()


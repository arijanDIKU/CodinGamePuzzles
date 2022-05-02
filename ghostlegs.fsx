open System

let token = (Console.In.ReadLine()).Split [|' '|]
let W = int(token.[0])
let H = int(token.[1])
let lines = [for i in 0 .. H - 1 do let line = Console.In.ReadLine() in line]


let symbols = [for i in 1..(W/3)+1 do i] // '|--'= 3 chars and +1 for the last column '|' so begin at 0


let traverse (l:string list) (symbols: int list) (W:int) : int list =
   let connectors = 0::[for i in 1..3..W-1 do if l.[i]="-" then 1 else 0]
   List.mapFold (fun prev c s -> if prev ) connectors symbols  
 

let rec solution (lines:string list) (symbols:int list) : int list =
    
    match lines with 
    | [] -> symbols 
    | lne::lnes ->  traverseLine lne |> solution lnes 
 

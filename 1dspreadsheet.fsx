//Level: easy 
open System


type Argument =
    | Reference of int 
    | Value of int
    | NotUsed

type Operation = 
    | VALUE 
    | ADD 
    | SUB 
    | MULT 

type Cell =
    | Computed of int
    | NonComputed of Argument*Operation*Argument 
    
        
    
module Parsing =
    let parseOpn (opn: string) : Operation =
            match opn with 
            | "ADD" -> ADD | "SUB" -> SUB | "MULT"-> MULT | "VALUE" -> VALUE | _ -> VALUE

    let parseArg (arg: string) : Argument =
        match arg.[0] with 
        | '$' -> Reference (int <| arg.[1..])
        | '_' -> NotUsed
        |  _  -> Value (int <| arg)

    let parse (a1,opn,a2) : Cell = 
        NonComputed (parseArg a1, parseOpn opn, parseArg a2)

module Evaluation =
    
    let evalOpn (cell:Cell) : int = 
        match cell with 
        | Computed n -> n
        | NonComputed (a1,opn,a2) -> match opn with 
                                     | VALUE -> match a1 with Value v -> v | _ -> -1
                                     | ADD   -> match (a1,a2) with (Value v1, Value v2) -> v1+v2 | _ -> -1
                                     | SUB   -> match (a1,a2) with (Value v1, Value v2) -> v1-v2 | _ -> -1
                                     | MULT  -> match (a1,a2) with (Value v1, Value v2) -> v1*v2 | _ -> -1 
        
    
    let evalCell (cell : Cell) : Cell =
        match cell with
        | Computed v -> cell
        | NonComputed (a1,opn,a2) -> 
                            match a1,a2 with
                            | Reference n, _ | _, Reference n -> cell //NonComputed
                            | _ -> Computed (evalOpn cell)
                                   

let solution (spreadsheet : Cell list) : Cell list =
    let memStack = [0..List.length spreadsheet - 1]
    
    let rec traverse (spreadsheet : Cell list) (memStack : int list) : Cell list =
        printfn "%A %A" spreadsheet memStack
        match memStack with 
        | [] -> spreadsheet 
        | i::indexes -> match Evaluation.evalCell spreadsheet.[i] with 
                        | Computed v -> traverse (spreadsheet.[0..i-1]@[Computed v]@spreadsheet.[i+1..]) indexes 
                        | NonComputed (a1,opn,a2) -> match a1,a2 with //can't compute
                                                     | Reference n, Reference m -> 
                                                        let spreadsheet' = traverse spreadsheet [n] //compute the reference value or check it is already computed 
                                                        let refRes1 : Argument = match spreadsheet'.[n]  with | Computed v1 -> Value v1 | _ -> Value -1  
                                                        let spreadsheet'' = traverse spreadsheet' [m]
                                                        let refRes2 : Argument = match spreadsheet''.[m] with | Computed v2 -> Value v2 | _ -> Value -1

                                                        let spreadsheetDeref = (spreadsheet''.[0..i-1]@[NonComputed (refRes1, opn, refRes2)]@spreadsheet''.[i+1..])

                                                        traverse spreadsheetDeref memStack //sent it back again

                                                     | Reference n, _ -> 
                                                        let spreadsheet' = traverse spreadsheet [n] 
                                                        let refRes : Argument = match spreadsheet'.[n] with | Computed v -> Value v | _ -> Value -1 

                                                        let spreadsheetDeref = (spreadsheet'.[0..i-1]@[NonComputed (refRes, opn, a2)]@spreadsheet'.[i+1..]) 

                                                        traverse spreadsheetDeref memStack //sent it back again    
                                                     | _, Reference m ->
                                                        let spreadsheet' = traverse spreadsheet [m] 
                                                        let refRes : Argument = match spreadsheet'.[m] with | Computed v -> Value v | _ -> Value -1 

                                                        let spreadsheetDeref = (spreadsheet'.[0..i-1]@[NonComputed (a1, opn, refRes)]@spreadsheet'.[i+1..]) 

                                                        traverse spreadsheetDeref memStack //sent it back again    
    traverse spreadsheet memStack 
                                                     
                                                                       


let N = int <| Console.In.ReadLine() 
let spreadsheet : Cell list = 
    [for i in 0 .. N-1 do yield let token = (Console.In.ReadLine()).Split [|' '|] in 
                                              let operation = token.[0] in 
                                              let arg1 = token.[1] in 
                                              let arg2 = token.[2] in 
                                              Parsing.parse (arg1,operation,arg2):Cell
    ] 
solution spreadsheet |> List.map (fun cell -> match cell with 
                                              | Computed c -> printfn "%A" c
                                              | _ -> printfn "should never happen"  
                                 )

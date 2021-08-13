// Classic puzzle 
// Level easy 

open System

let N = int(Console.In.ReadLine()) (* Number of elements which make up the association table. *)
let Q = int(Console.In.ReadLine()) (* Number Q of file names to be analyzed. *)

let input = seq {for n in 0 .. N-1 do yield let token = (Console.In.ReadLine()).Split [|' '|] in 
                                                token.[0], token.[1] //ext and mimetype respectively
                            }
let associationTable = Map.ofSeq input


let containsDot fname = 
    fname |> String.exists (fun c -> c='.')

let getFileExt (fname:string) : string =
    let index = fname.LastIndexOf "."
    fname.[index+1..]

let lookupFilename (associationTable: Map<string,string>) (fname:string): unit =
    match (containsDot fname) with 
    | false -> printfn "UNKNOWN"
    | true ->
        let fileExt = getFileExt fname
        match associationTable.TryFind (fileExt.ToUpper()), associationTable.TryFind (fileExt.ToLower()) with
        | (Some mimetype, None) -> printfn "%s" mimetype
        | (None, Some mimetype) -> printfn "%s" mimetype
        | (None, None) -> printfn "UNKNOWN"


let filenames = [for i=0 to Q-1 do yield Console.In.ReadLine()]
let solution = lookupFilename associationTable                 //returns partially applied function

filenames |> List.map solution 



//Level: easy


open System

type Point = {
    long: float;
    lat: float
    }

type nameNpoint = string*Point


let distFormula (A : Point) (B : Point) : float = // is given in problem description
    let { long=longA; lat = latA } = A
    let { long=longB; lat = latB } = B
    let x = (longB - longA) * cos ((latA+latB)/2.0)
    let y = latB - latA
    ((x**2.0) + (y**2.0)) |> sqrt |> ( * ) 6371.0

let convertToFloat (f:string) : float =
    f |> String.map (fun c -> if c=',' then '.' else c) |> float

let getFields (defibrillator:string) : nameNpoint = //get position of defibrillator by converting string representation to float
    let fields = Seq.toList (defibrillator.Split ';')
    let x = convertToFloat fields.[4]   
    let y = convertToFloat fields.[5]
    fields.[1],{long=x; lat=y}

let LON = convertToFloat (Console.In.ReadLine())
let LAT = convertToFloat (Console.In.ReadLine())
let N = int(Console.In.ReadLine())
let defibrillators = [for i in 0 .. N-1 do yield Console.In.ReadLine()]
let userPos = {long=LON; lat=LAT}

let computeDist = distFormula userPos //returns partially applied function

let typedFields = defibrillators |> List.map getFields 
let distances = typedFields |> List.map (fun e -> snd e) |> List.map computeDist
let minValue = List.min distances 
let index = List.findIndex (fun elm -> elm=minValue) distances 

printfn "%s" (fst typedFields.[index])

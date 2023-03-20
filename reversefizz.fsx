open System


type FizzBuzz = 
    F
    | B
    | FB
    | N of int

let n = int(Console.In.ReadLine())


let lines = [ for i in 0 .. n - 1 do Console.In.ReadLine() ] |> List.map (fun x -> match x with
                                                                                   | "Fizz" -> F
                                                                                   | "Buzz" -> B
                                                                                   | "FizzBuzz" -> FB
                                                                                   | _ -> N (int x))



let divs n = 
    let s = n/2 in Seq.init s (fun i -> i+1) |> Seq.filter (fun i -> if n%i=0 then true else false) |> (fun s -> Seq.append s [n]) 

let add candidates n = 
    let candidates' = divs n in Set.ofSeq candidates' |> Set.union candidates 

let diff candidates n = 
    let excluded = divs n in Set.ofSeq excluded |> Set.difference candidates  

let inter candidates n =
    let candidates' = divs n in Set.ofSeq candidates' |> Set.intersect candidates  


let init lines offset = 
    Seq.findIndex (function | F | FB -> true | _ -> false) lines + offset |> divs |> Set.ofSeq
    ,
    Seq.findIndex (function | B | FB -> true | _ -> false) lines + offset |> divs |> Set.ofSeq 
    ,
    offset 

let solve lines =   
    let offset = lines |> Seq.indexed |> Seq.tryFind (function (i, N n) -> true | _ -> false) |> (function None -> 1 | Some (i,x) -> match x with N n -> n-i | _ -> failwith "never") in 
        Seq.fold (fun (fCand,bCand,i) l -> 
                                            match l with 
                                            | F -> inter fCand i, diff bCand i, i+1 
                                            | B -> diff fCand i, add bCand i, i+1 
                                            | FB -> inter fCand i, inter bCand i, i+1 
                                            | N _ -> diff fCand i, diff bCand i, i+1) (init lines offset) lines

solve lines |> (fun (f,b,_) -> printf $"{Seq.head f} {Seq.head b}")

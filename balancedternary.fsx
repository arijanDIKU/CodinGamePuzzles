open System

let N = int(Console.In.ReadLine())

let rec convert2unbl n m =
    match n with 
    | 0 -> m 
    | _ -> let d = n%3 |> string in convert2unbl (n/3) (d+m)


let convert2bln (m:string) = 
    let x = Seq.append "0" m in 
        Seq.mapFoldBack (fun d s -> 
                                    match s with 
                                    | 0 -> if d='2' then ('T',1) else d,0   
                                    | 1 -> match d with 
                                                        | '0' -> '1', 0 
                                                        | '1' -> 'T',1
                                                        | '2' -> '0', 1) x 0 |> fst |> Seq.skipWhile (fun c -> c='0') |> System.String.Concat 


if N = 0 then 
    printf "0"
else if N < 0 then 
    let r = convert2unbl (abs N) "" |> convert2bln |> Seq.map (function | '0' -> '0' | '1' -> 'T' | 'T' -> '1') in 
        System.String.Concat r |> printfn "%s"
else 
    convert2unbl N "" |> convert2bln |> printfn "%s" 

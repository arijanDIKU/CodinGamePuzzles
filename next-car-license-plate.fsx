open System

let x = Console.In.ReadLine()
let n = int(Console.In.ReadLine())

let i,j = 26*26, 999 //998 because modulo starts from 0
let xs = ['A'..'Z'] |> List.map string
let ys = seq { for a in xs do yield! Seq.map2 (+) (Seq.replicate 26 a) xs}
let offset (letterPair:string) = //index of letter combination in a 1d list representation of all combinations
    26*(int letterPair.[0] - int 'A') + (int letterPair.[1] - int 'A')

let o1,o2,o3 = x.Split[|'-'|] |> (fun y -> offset y[0], int y[1]-1, offset y[2]) 
let res2 = ((o2+n)%j)+1
let res1,res3 = 
    let m = (o2+n)/j in let l = m/i in 
        (o1+l)%i, (o3+m)%i



let result = (Seq.item res1 ys, res2, Seq.item res3 ys) in 
    result |||> printfn "%s-%03i-%s"      

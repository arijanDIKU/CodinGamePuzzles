open System

let operation = Console.In.ReadLine()
let pseudoRandomNumber = int(Console.In.ReadLine())
let rotors = [ for i in 0 .. 3 - 1 do Console.In.ReadLine() ]

let message = Console.In.ReadLine()
let alphabets = List.init 3 (fun _ -> seq {'A'..'Z'} |> System.String.Concat)

let caesarshift rnd (op:int->int->int) (msg:string) = 
        Seq.mapFold (fun i c -> let newchar = int c + rnd + i
                                if  newchar < int 'Z'+1 && newchar > int 'A' - 1 then 
                                        char newchar,op i 1
                                elif newchar > int 'Z' then 
                                        let n = int '@' + (newchar%int 'Z')%26 in char n,op i 1 
                                else //newchar < int 'A'
                                        let n = int 'Z' - (int 'A' - newchar-1)%26 in char n,op i 1) 0 msg 
 
        |> fst

let encode (alphabet:string list) (rotors:string list) (msg:char seq) = 
        let cshift = msg |> System.String.Concat in
            List.fold2 (fun csr alph rtr -> String.map (fun c -> let index = Seq.findIndex ((=) c ) alph in  
                                                                        Seq.item index rtr) 
                                                                 csr) 
                                                                cshift alphabet rotors 
                                                    
let decode (rnd:int) (msg:string) = 
    encode (List.rev rotors) alphabets msg 
    |> caesarshift (-rnd) (-) |> System.String.Concat

if operation="ENCODE" then 
        caesarshift pseudoRandomNumber (+) message
        |> encode alphabets rotors |> printfn "%s"
else 
        decode pseudoRandomNumber message |> printfn "%s"

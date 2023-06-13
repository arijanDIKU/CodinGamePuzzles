(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
open System

type Cfg = 
    { 
        Arrows : int;
        Height : int;
        Thickness : int; 
        Spacing : int;
        Indent : int;
    }



let dir, cfg = 
    let input = Console.In.ReadLine().Split(" ") in
        input[0], 
        { 
        Arrows = int input[1];
        Height = int input[2];
        Thickness = int input[3]; 
        Spacing = int input[4];
        Indent = int input[5];
        }



let interval,symbol = 
    match dir with 
    | "right" -> (List.rev [0..cfg.Height/2]) @ [1..cfg.Height/2],">"
    | "left" -> List.init cfg.Height id,"<"
    | _ -> failwith "->"


List.iter (fun i -> let diff = abs (cfg.Height/2 - i) in 
                        let headerArrow = String.init cfg.Thickness (fun _ -> symbol) in 
                            let line =  String.init (cfg.Arrows-1) (fun _ -> 
                                                                            String.init cfg.Spacing (fun _ -> " ")  
                                                                            + 
                                                                            String.init cfg.Thickness (fun _ -> symbol) ) in 
                                let indentation = String.init (diff*cfg.Indent) (fun _ -> " ") in 
                                        printfn "%s" (indentation+headerArrow+line)
        ) interval

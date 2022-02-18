pen System

let BEGIN = Console.In.ReadLine()
let END = Console.In.ReadLine()


let solution (t1: string) (t2: string) : unit = 
    let begin' = DateTime(int <| t1.[6..], int <| t1.[3..4], int <| t1.[0..1])
    let end' = DateTime(int <| t2.[6..], int <| t2.[3..4], int <| t2.[0..1])
    let result = end' - begin' 
    match (int <| result.TotalDays) with 
    | x when x=0 -> printfn "total 0 days"
    | x when x<31 -> printfn "total %i days" (int <| result.TotalDays)
    | x when x=31 -> printfn "1 month, total 31 days"
    | x when x<365 -> 
            let numberOfMonths = end'.Month-begin'.Month
            let monthSpelling = (+) "month" (if numberOfMonths=1 then "" else "s")
            printfn "%i %s, total %i days" numberOfMonths monthSpelling (int <| result.TotalDays)
    | x when x=365 -> printfn "1 year, total 365 days" 
    | _ ->  
            let numberOfMonths = 
                let monthDiff = (end'.Month-begin'.Month)
                if monthDiff<=0 then 
                    (int <| (end'-DateTime(end'.Year,begin'.Month,begin'.Day)).TotalDays) / 30
                    |> (+) 12
                else 
                    (int <| (end'-DateTime(end'.Year,begin'.Month,begin'.Day)).TotalDays) / 30
                                                    
            let monthSpelling = (+) "month" (if numberOfMonths=1 then "" else "s")
            let numberOfYears = (int <| result.TotalDays/365.0)
            let yearSpelling = (+) "year" (if numberOfYears=1 then "" else "s")
            if end'.Month=begin'.Month then 
                printfn "%i %s, total %i days" numberOfYears yearSpelling (int <| result.TotalDays)
            else 
                printfn "%i %s, %i %s, total %i days" numberOfYears yearSpelling numberOfMonths monthSpelling (int <| result.TotalDays)




solution BEGIN END

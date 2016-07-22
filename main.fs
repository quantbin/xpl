open config
open utils
open interp
[<EntryPoint>]
let main argv = 
    Console.WindowHeight<-int(float Console.LargestWindowHeight*0.75)
    Console.WindowWidth<-int(float Console.LargestWindowWidth*0.5)
    if File.Exists("history.txt") then HIST<-File.ReadLines("history.txt")|>List.ofSeq
    while not DONE do

        let testExpr="1 2 3;map x {i:mul i 2;for . {j:nop j}}; prn ."
        //let testExpr="1 2 3;set x .;map x {i:mul i 2;for . {j:nop j}}; prn ."
        //let testExpr="set x .;map x {i:* i 2}; prn ."

        Console.Write(">")
        try
            let cmd=Console.ReadLine()
            //interp cmd|>ignore
            interp testExpr|>ignore
        with
            |ex->printfn "*** error: %s" ex.Message
    File.WriteAllText("history.txt",HIST|>Seq.take(Math.Min(200, HIST.Length))|>String.concat("\r\n"));
    0

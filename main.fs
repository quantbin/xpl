open Microsoft.Office.Interop.Excel
open config
open utils
open interp
[<EntryPoint>]
let main argv = 
    Console.WindowHeight<-int(float Console.LargestWindowHeight*0.75)
    Console.WindowWidth<-int(float Console.LargestWindowWidth*0.5)
    if File.Exists("history.txt") then HIST<-File.ReadLines("history.txt")|>List.ofSeq
    while not DONE do
        if not DEBUG then Console.Write(">") else Console.Write("DEBUG>") 
        try
            let cmd=Console.ReadLine().Trim()
            interpLine cmd
        with
            |ex->printfn "*** error: %s" ex.Message
    File.WriteAllText("history.txt",HIST|>Seq.take(Math.Min(200, HIST.Length))|>String.concat("\r\n"));
    if not (null=EXCEL) then 
        //http://www.xtremevbtalk.com/tutors-corner/160433-automating-office-programs-vb-net-com-interop.html
        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()
        //???
        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()
        let wb=EXCEL.ActiveWorkbook
        let ws=EXCEL.ActiveSheet:?>Worksheet
        System.Runtime.InteropServices.Marshal.FinalReleaseComObject(ws)|>ignore
        System.Runtime.InteropServices.Marshal.FinalReleaseComObject(wb)|>ignore
        Marshal.ReleaseComObject EXCEL|>ignore
    0

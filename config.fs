module config
//system
type File=System.IO.File
type Math=System.Math
type Directory=System.IO.Directory
type Console=System.Console
type Marshal=System.Runtime.InteropServices.Marshal
type Type=System.Type
//interop
type Application=Microsoft.Office.Interop.Excel.Application
type Worksheet=Microsoft.Office.Interop.Excel.Worksheet
type Range=Microsoft.Office.Interop.Excel.Range
type ApplicationClass=Microsoft.Office.Interop.Excel.ApplicationClass
type XlReferenceStyle=Microsoft.Office.Interop.Excel.XlReferenceStyle
type XlLookAt=Microsoft.Office.Interop.Excel.XlLookAt
type XlSearchDirection=Microsoft.Office.Interop.Excel.XlSearchDirection
type XlFindLookIn =Microsoft.Office.Interop.Excel.XlFindLookIn 
//types
type Proc={arg:string list;body:string}
type Rng={top:int;left:int;width:int;height:int}
type Mtx={mtxData:obj [,]}
type KitchenSink=
    |F of float
    |I of int
    |S of string
    |R of Rng
    |M of Mtx //matrix
    |C of KitchenSink list //collection of kitchen sink objects
    |GV of string
    |LV of string
    |EmptyGV of string
    |EmptyLV of string
    |EmptySink
//globals
let HELP="
EXIT            exit shell
HIST            show commands history
NEW             start new Excel instance
CELL 1 1        create reference to cell
RANG 1 1 2 2    create reference to range
NEW             open new excel workbook
-> n v
<- v n          assignment
PRN $x          print var $x
+-*/            arithmetic
@ $x            value of var $x
>R N            move range N steps to the right (or left if N is negative)
>R N            move range N steps down (or up if N is negative)
? ...           search for string in active sheet; returns collection of matched cells
...\...         eval left side - it should return a collection; then exec right side on each elem of collection
"
let mutable HIST=[""]
let mutable EXCEL:Application=null
let mutable DONE=false
///map of local variables that are valid for current line only
let mutable LOCALS=new System.Collections.Generic.Dictionary<string,KitchenSink>()
///map of global variables
let mutable GLOBALS=new System.Collections.Generic.Dictionary<string,KitchenSink>()
let mutable DEBUG=false
let mutable PROCS=new System.Collections.Generic.Dictionary<string,Proc>()

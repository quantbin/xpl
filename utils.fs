module utils
open config
let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [for g in m.Groups -> g.Value])
    else None
let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None
let concat t=t|>String.concat " "
let panic s=raise(new System.Exception(s))
let prodI x=
    x|>Seq.fold(fun a e->a*e) 1
let prodF x=
    x|>Seq.fold(fun a e->a*e) 1.0
///is string float?
let strIsF x=
    match x with
    |Regex "[0-9]+\."[]
    |Regex "\.[0-9]+"[]
    |Regex "[0-9]+\.[0-9]+"[]->true
    |_->false
///see if all kitchen sink objects are of F type
let areF (xx)=xx|>Seq.forall(fun x->match x with |F x->true|_->false)
///is string int?
let strIsI x=
    match x with
    |Regex "[0-9]+" []->true
    |_->false
///kitchen sink type checkers
let rec isI x=
    match x with
    |I x->true
    |LV x->isI LOCALS.[x]
    |GV x->isI GLOBALS.[x]
    |_->false
let rec isF x=
    match x with
    |F x->true
    |LV x->isF LOCALS.[x]
    |GV x->isF GLOBALS.[x]
    |_->false
let rec isS x=
    match x with
    |S x->true
    |LV x->isS LOCALS.[x]
    |GV x->isS GLOBALS.[x]
    |_->false
let rec isR x=
    match x with
    |R x->true
    |LV x->isR LOCALS.[x]
    |GV x->isR GLOBALS.[x]
    |_->false
///kitchen sink getters
let rec getR x=
    match x with
    |R x->x
    |LV x->getR LOCALS.[x]
    |GV x->getR GLOBALS.[x]
    |_->panic(sprintf "type error; %A is not range" x)
let rec getF x=
    match x with
    |F x->x
    |LV x->getF LOCALS.[x]
    |GV x->getF GLOBALS.[x]
    |_->panic(sprintf "type error; %A is not float" x)
let rec getI x=
    match x with
    |I x->x
    |LV x->getI LOCALS.[x]
    |GV x->getI GLOBALS.[x]
    |_->panic(sprintf "type error; %A is not int" x)
let rec getS x=
    match x with
    |S x->x
    |LV x->getS LOCALS.[x]
    |GV x->getS GLOBALS.[x]
    |_->panic(sprintf "type error; %A is not string" x)
let rec getAsS v=
    match v with
    |R(x)->sprintf "RNG: Left:%d, Top:%d, Width:%d, Height:%d" x.left x.top x.width x.height
    |GV(x)->
        match GLOBALS.[x] with
        |R r->sprintf "Glob Var pointing to RNG: Left:%d, Top:%d, Width:%d, Height:%d" r.left r.top r.width r.height
        |_->sprintf "Glob Var: %A" GLOBALS.[x]
    |LV(x)->
        match LOCALS.[x] with
        |R r->sprintf "Local Var pointing to RNG: Left:%d, Top:%d, Width:%d, Height:%d" r.left r.top r.width r.height
        |_->sprintf "Local Var: %A" LOCALS.[x]
    |EmptyGV(x)->sprintf "uninitialized Globsl Var: %s" x
    |EmptyLV(x)->sprintf "uninitialized Local Var: %s" x
    |I x->sprintf "I:%d" x
    |F x->sprintf "F:%f" x
    |S x->sprintf "S:%s" x
    |M x->sprintf "M:%A" x
    |C x->sprintf "C:%d elements" x.Length
    |_->panic("cannot read value from this type")
///is this range a cell?
let rngIsCel r=
    if (1=r.height)&&(1=r.width) then true else false
///are all kitchen sink objects of I type?
let areI (xx)=xx|>Seq.forall(fun x->match x with |I(x)->true|_->false)
///is string numeric?
let isNum x=(strIsF x)||(strIsI x)
///guess type and convert string to corresponding kitchen sink obj
let guessTyp x=
    match x with
    |Regex "^[0-9]+$" []->I(int x)
    |Regex "^[0-9]+\.$"[]->F(float x)
    |Regex "^\.[0-9]+$"[]->F(float x)
    |Regex "^[0-9]+\.[0-9]+$"[]->F(float x)
    |Regex "^\$$"[]->GV x
    |Regex "^_$"[]->LV x
    |_->S(x)
///parse arg strings and cast to proper types; return seq of kitchen sinks
let parseArg argTyp=
    let r=[]
    argTyp
    |>Seq.map(fun (a:string,t)->
        if a.StartsWith "_" then
            if LOCALS.ContainsKey a then LV a else EmptyLV a
        else if a.StartsWith "$" then
            if GLOBALS.ContainsKey a then GV a else EmptyGV a
        else
            match t with
            |"F"->F(float a)
            |"I"->I(int a)
            |"N"->if strIsF a then F(float a) else if strIsI a then I(int a) else panic("value not numeric: "+a)
            |"S"->S a
            |"?"->guessTyp a
            |_->panic("unknown type: "+t)
    )
///print value
let prnValue v=
    printfn "%s" (getAsS v)
let setRngVal (r:Rng) (value:KitchenSink)=
    let ws=EXCEL.ActiveSheet:?>Worksheet
    if rngIsCel r then
        let rng=ws.Cells.[r.top, r.left]:?>Range
        match value with 
        |F x->rng.Value2<-x
        |I x->rng.Value2<-x
        |S x->rng.Value2<-x
        |_->panic("cell write op supports only int, float and string types")
    else
        let tl=ws.Cells.[r.top, r.left]:?>Range
        let br=ws.Cells.[r.top+r.height-1,r.left+r.width-1]:?>Range
        let rng=ws.Range(tl,br)
        match value with 
        |M x->rng.Value2<-x
        |_->panic("range write op supports matrix type")
///get range value (matrix)
let getRngVal (r:Rng)=
    let ws=EXCEL.ActiveSheet:?>Worksheet
    let rng=
        if rngIsCel r then
            ws.Cells.[r.left, r.top]:?>Range
        else
            let tl=ws.Cells.[r.left, r.top]:?>Range
            let br=ws.Cells.[r.left+r.width-1, r.top+r.height-1]:?>Range
            ws.Range(tl,br)
    let value=rng.Value2
    match value with
    | :? float as x->F(float x)
    |_->M({mtxData=value:?>obj[,]})
///panic if in edit mode
let panicIfEditMode()=
    try
        if not (null=EXCEL) then
            EXCEL.Goto("###")
    with
        |ex->if ex.Message.StartsWith("Exception") then panic("excel seems to be in edit mode; finish editing current cell and try again")
///print local and global variables
let prnVars()=
    printfn "LOCALS:"
    for e in LOCALS do
        printfn "  %s=%s" e.Key (getAsS e.Value)
    printfn "GLOBALS:"
    for e in GLOBALS do
        printfn "  %s=%s" e.Key (getAsS e.Value)

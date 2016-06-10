module interp
open Microsoft.Office.Interop.Excel
open config
open utils
///interpret function and its arguments (right set of arguments is coming from previous function)
let interpFun (func:string) argL (argRightKS:KitchenSink list)=
    let argLeftKS=argL|>Seq.map(fun a->(a,"?"))|>parseArg|>List.ofSeq
    let argAllKS=
        if not (0=(Seq.length argRightKS)) then
            List.concat [argLeftKS;argRightKS]
        else
            argLeftKS
    match func.ToUpper() with
    |"HELP"->printfn "%s" HELP;[]
    |"EXIT"->DONE<-true;[]
    |"DBG"->DEBUG<-not DEBUG;[]
    |"VARS"->prnVars();argAllKS
    |"HIST"->HIST|>Seq.rev|>Seq.iter(fun e->printfn "%s" e);[]
    |"+"|"*"->
        if areF argAllKS then 
            if "+"=func then [F (argAllKS|>Seq.map getF|>Seq.sum)]
            else [F (argAllKS|>Seq.map getF|>prodF)]
        else if areI argAllKS then 
            if "+"=func then [I (argAllKS|>Seq.map getI|>Seq.sum)]
            else [I (argAllKS|>Seq.map getI|>prodI)]
        else
            panic("arguments should all be either float or int")
    |"-"|"/"->
        if not (2=List.length argAllKS) then panic("expect 2 arguments")
        if areF argAllKS then
            let r=
                if "-"=func then  
                    (getF (argAllKS.Item 0))-(getF (argAllKS.Item 1))
                else
                    (getF (argAllKS.Item 0))/(getF (argAllKS.Item 1))
            [F r]
        else if areI argAllKS then 
            let r=
                if "-"=func then  
                    (getI (argAllKS.Item 0))-(getI (argAllKS.Item 1))
                else
                    panic("cannot divide integers; use floats")
            [I r]
        else
            panic("arguments should all be either float or int")
    |"CELL"->
        if not (2=argAllKS.Length) then panic("expect 2 arguments")
        if not (areI argAllKS) then panic("arguments should be int")
        if argAllKS|>Seq.exists(fun e->(getI e)<1) then panic("arguments should be positive")
        let r={top=getI argAllKS.[0];left=getI argAllKS.[1];width=1;height=1}
        [R r]
    |"RNG"->
        if not (4=argAllKS.Length) then panic("expect 4 arguments")
        if not (areI argAllKS) then panic("arguments should be int")
        if argAllKS|>Seq.exists(fun e->(getI e)<1) then panic("arguments should be positive")
        let r={top=getI argAllKS.[0];left=getI argAllKS.[1];width=getI argAllKS.[2];height=getI argAllKS.[3]}
        [R r]
    |"COOR"->
        if not (1=argAllKS.Length) then panic("expect 1 argument")
        if not (isR argAllKS.[0]) then panic("expect range as an argument")
        let r=getR argAllKS.[0]
        [I r.left;I r.top;I r.width;I r.height]
    |">R"->
        if not (2=argAllKS.Length) then panic("expect 2 arguments")
        if not (isI argAllKS.[0]) then panic("1st argument should be int (travel distance)")
        let dist=getI argAllKS.[0]
        if not (isR argAllKS.[1]) then panic("2nd argument should be Range")
        let r=getR argAllKS.[1]
        //move range
        if r.left+dist<1 then panic("left corner went beyond the left border")
        let rm={top=r.top;left=r.left+dist;width=r.width;height=r.height}
        [R rm]
    |">D"->
        if not (2=argAllKS.Length) then panic("expect 2 arguments")
        if not (isI argAllKS.[0]) then panic("1st should be int (travel distance)")
        let dist=getI argAllKS.[0]
        if not (isR argAllKS.[1]) then panic("2nd param should be Range")
        let r=getR argAllKS.[1]
        //move range
        if r.top+dist<1 then panic("top corner went beyond the top border")
        let rm={top=r.top+dist;left=r.left;width=r.width;height=r.height}
        [R rm]
    |"PRN"->
        argAllKS|>Seq.iter prnValue
        []
    |"?"->
        panicIfEditMode()//do not perform search when cell is in edit mode
        if not (1=argAllKS.Length) then panic("expect 1 argument")
        let ws=EXCEL.ActiveSheet:?>Worksheet
        let mutable r=ws.Cells.Find(
                        getAsS argAllKS.[0],
                        Type.Missing,
                        Type.Missing,
                        XlLookAt.xlWhole,
                        Type.Missing,
                        XlSearchDirection.xlNext,
                        true,
                        Type.Missing,
                        Type.Missing)
        let firstMatchAddr=if null=r then null else r.AddressLocal(true,true,XlReferenceStyle.xlA1,Type.Missing,Type.Missing)
        let mutable rr=[]
        while not (null=r) do
            rr<-R {
                top=r.Row;
                left=r.Column;
                width=1;
                height=1
            }::rr
            r<-ws.Cells.FindNext(r)
            let currMatchAddr=if null=r then null else r.AddressLocal(true,true,XlReferenceStyle.xlA1,Type.Missing,Type.Missing)
            if firstMatchAddr=currMatchAddr then 
                r<-null
        [C(rr)]
    |"@"->
        if not (1=argAllKS.Length) then panic("expect 1 argument")
        let var=argAllKS.[0]
        match var with
        |R(x)->[getRngVal x]
        |GV(x)->
            match GLOBALS.[x] with
            |R r->[getRngVal r]
            |_->[GLOBALS.[x]]
        |LV(x)->
            match LOCALS.[x] with
            |R r->[getRngVal r]
            |_->[LOCALS.[x]]
        |EmptyGV(x)->panic("var is uninitialized")
        |EmptyLV(x)->panic("var is uninitialized")
        |_->panic("cannot read value from this type")
    |"<-"|"->"->
        let halfLen=argAllKS.Length/2
        if not (0=(argAllKS.Length%2)) then panic("expect even number of arguments")
        let values=match func with
                    |"<-"->argAllKS.[0..halfLen-1]
                    |"->"->argAllKS.[halfLen..]
                    |_->panic("no mans land")
        let dest=match func with
                    |"<-"->argAllKS.[halfLen..]
                    |"->"->argAllKS.[0..halfLen-1]
                    |_->panic("no mans land")
        List.zip dest values|>Seq.iter(fun (d,v)->
            match d with
            |R(x)->setRngVal x v
            |GV(x)->
                match GLOBALS.[x] with
                |R r->
                    match v with
                    |R y->GLOBALS.[x]<-v //we are assigning range to a range; update current range def
                    |_->setRngVal r v //we are assigning value to a range; update value in excel
                |_->GLOBALS.[x]<-v
            |LV(x)->
                match LOCALS.[x] with
                |R r->
                    match v with
                    |R y->LOCALS.[x]<-v //we are assigning range to a range; update current range def
                    |_->setRngVal r v //we are assigning value to a range; update value in excel
                |_->LOCALS.[x]<-v
            |EmptyGV(x)->GLOBALS.[x]<-v
            |EmptyLV(x)->LOCALS.[x]<-v
            |S(x) when "."=x->()//if destination is '.' - ignore this assignment
            |_->panic("wrong assignment target (did you forget $ or _ in front of the variable name?)")
            |>ignore
        )
        []
    |""->[]//NOP; ignore right args (stop them from escaping)
    |"NEW"->
        EXCEL<-ApplicationClass(Visible=true)
        EXCEL.Workbooks.Add(XlWBATemplate.xlWBATWorksheet)|>ignore
        []
    |_->
        //parse left args again - this time include function (interpret it as a var or const now)
        let argLeftKsPlusFun=func::argL|>Seq.map(fun a->(a,"?"))|>parseArg|>List.ofSeq
        if not (0=(Seq.length argRightKS)) then
            List.concat [argLeftKsPlusFun;argRightKS]
        else
            argLeftKsPlusFun
///interpret single expression
let rec interpExpr (expr:string) (arg:KitchenSink list)=
    if DEBUG then
        printfn "eval expr: %s" expr
        printfn "input:"
        arg|>Seq.iter prnValue
    let r=
        match expr.Split [|' '|]|>List.ofArray with
        |h::[]->interpFun h [] arg
        |h::r->interpFun h r arg
        |[]->[]
    //do not debug dbg command
    if DEBUG && not ("DBG"=expr.ToUpper()) then
        printfn "output:"
        r|>Seq.iter prnValue
        prnVars()
        printfn "press Enter key to kontinue, Q then Enter to exit debug mode..."
        if "Q"=Console.ReadLine().Trim() then DEBUG<-false
    r
///interpret line with | separated expressions 
let interpExprns (expr:string) (argIn)=
    let mutable arg=[]
    if not (argIn=EmptySink) then arg<-[argIn]
    expr.Split [|'|'|]
    |>Seq.iter(fun expr ->
        arg<-interpExpr (expr.Trim()) arg
    )
    arg
///interpret line; see if it contains EACH operator, if yes - eval left side and iterate over it with the right side;
///if no EACH operator is present, eval the whole line
let interpLine (line:string)=
    if not (line=HIST.Head) then HIST<-line::HIST
    let linePad=line
                    .Replace("->", " -> ").Replace("<-", " <- ").Replace("@", " @ ").Replace("?", " ? ")
                    .Replace("  ", " ")

    match linePad with 
    |Regex "(?i)PROC ([^ ]+) ([^:]+):(.*)"[name;arg;body]->
        //this is proc declaration, something like: proc setto555 _x _y:CELL _x _y|<-555
        PROCS.[name]<-{arg=arg.Split [|' '|]|>List.ofArray;body=body}
    |_->
        if linePad.Contains("\\") then
            //EACH op in present; find WHAT (collection) and HOW (fun to apply to each elem of collection) parts
            let whatAndHow=linePad.Split [|'\\'|]
            let what=whatAndHow.[0]
            let how=whatAndHow.[1]
            let arg=interpExprns what EmptySink
            //here arg should contain the collection over which we will iterate
            if 1<arg.Length then panic("EACH takes exactly one parameter - collection to iterate over")
            match arg.[0] with
            //EACH is performed on collection
            |C x->x|>Seq.iter(fun a->interpExprns (how.Trim()) a|>ignore)
            |GV x ->
                match GLOBALS.[x] with
                //EACH is performed on collection stored in global var
                |C x2->x2|>Seq.iter(fun a->interpExprns (how.Trim()) a|>ignore)
                |_->panic("EACH can only be performed on collections")
            |LV(x)->
                match LOCALS.[x] with
                //EACH is performed on collection stored in local var
                |C x2->x2|>Seq.iter(fun a->interpExprns (how.Trim()) a|>ignore)
                |_->panic("EACH can only be performed on collections")
            |_->panic("EACH can only be performed on collections")
        else
            //no EACH in this line; interpret the whole thing; start with empty sink
            interpExprns linePad EmptySink|>ignore
    LOCALS.Clear()
    ()

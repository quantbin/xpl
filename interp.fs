module interp
open config
open utils
open System
///interpret script
type State=
    |START|WHITESPACE|TOKEN|SEPARATOR
let processToken tokenStr=
    match tokenStr with
    //literals
    |Regex "[0-9]+" []->LITERAL(INT (int tokenStr))
    |Regex "[0-9]*\.[0-9]+" []->LITERAL(FLOAT (float tokenStr))
    |"true"->LITERAL(BOOL true)
    |"false"->LITERAL(BOOL false)
    |Regex "[0-9][0-9][0-9][0-9]\.[0-9][0-9].[0-9][0-9]" []->
        LITERAL(TIME(DateTime.ParseExact("yyyy.MM.dd",tokenStr,culture)))
    |Regex "[0-9][0-9][0-9][0-9]\.[0-9][0-9].[0-9][0-9]_[0-9][0-9].[0-9][0-9].[0-9][0-9]" []->
        LITERAL(TIME(DateTime.ParseExact("yyyy.MM.dd_hh.mm.ss",tokenStr,culture)))
    |Regex "'(.*)" [x]->LITERAL(STR x)
    //vars;
    //var names always start with non-numeric char
    |Regex "[a-zA-Z][0-9a-zA-Z]*" []->VAR tokenStr
    |Regex "([a-zA-Z][0-9a-zA-Z]*)@([0-9]+)" [x;y]->VARIDX(x,int y)
    |Regex "([a-zA-Z][0-9a-zA-Z]*)@([a-zA-Z][0-9a-zA-Z]*)" [x;y]->VARMAP(x,y)
    |Regex "[\.]+" []->DOTS tokenStr.Length
    |Regex "([\.]+)@([0-9']+)+" [x;y]->DOTSIDX(x.Length,int y)
    |Regex "([\.]+)@([a-zA-Z][0-9a-zA-Z]*)" [x;y]->DOTSMAP(x.Length,y)
    |_->panic("unrecognized token: "+tokenStr)
let rec createAst (source:string) (offest:int):Expr list=
    let mutable tokenStart= -1
    let mutable tokenEnd= -1
    let mutable exprStart= -1
    let mutable exprEnd= -1
    let mutable expressions=[]
    let mutable tokens=[]
    let mutable currentState=START
    let mutable newState=START
    //iterate over source string chars and tokenize it (break into tokens/expressions)
    let mutable i=0
    while i<source.Length do
    //for i in 0..(source.Length-1) do
        newState<-
            match source.[i] with
            |';'|'\n'|'\r'->SEPARATOR
            |x when Char.IsWhiteSpace x->WHITESPACE
            |'{'->
                //find matching brace and pass lambda text to recursive interp call
                let mutable level=1
                for j in (i+1)..(source.Length-1) do
                    if '{'=source.[j] then
                        level<-level+1
                    if '}'=source.[j] then
                        level<-level-1
                        if 0=level then
                            //we found matching brace; process lambda (skip braces)
                            let lamStr=source.Substring(i+1,j-1-(i+1)+1)
                            printfn "lam: %s" lamStr
                            //split lambda into parameters and body
                            let paramsStr,lamBody=
                                match lamStr with
                                |Regex "([a-zA-Z ]):(.*)" [paramsStr;lamBody]->paramsStr,lamBody
                                |_->"",lamStr
                            let lamParams=paramsStr.Split([|' '|])|>List.ofArray
                            //process lambda body
                            let lam=createAst lamBody i
                            let lamToken=LAM(lamParams,COMPOSITE(lam,i+1+offest,j-1+offest))
                            tokens<-lamToken::tokens
                            //advance the index to char after lambda's }
                            i<-j
                            ()
                    if level>0 && i=(source.Length-1) then 
                        panic("matching } not found for { at pos: "+(string i))
                //we have finished parsing the lambda;=
                WHITESPACE
            |'}'->
                //shouldn't be here - matching } should have been found during lambda detection (previous step)
                panic("unmatched }")
            |_->TOKEN

        //process state transitions
        match (currentState,newState) with 
        |(START,TOKEN)->
            tokenStart<-i
            exprStart<-i
            ()
        |(START,WHITESPACE)->
            exprStart<-i
            ()
        |(START,SEPARATOR)->
            //empty expr; do nothing
            ()
        |(TOKEN,TOKEN)->
            //still on token; do nothing
            ()
        |(TOKEN,WHITESPACE)->
            //detected end of non-lambda token
            tokenEnd<-i
            let tokenStr=source.Substring(tokenStart, tokenEnd-tokenStart)
            //process token
            let token=processToken tokenStr
            tokens<-token::tokens
            tokenStart<- -1
            tokenEnd<- -1
            ()
        |(WHITESPACE,WHITESPACE)->
            //still on whitespace; do nothing
            ()
        |(WHITESPACE,TOKEN)->
            //start of new token
            tokenStart<-i
            ()
        |(_,SEPARATOR)->
            //see if we are finishing not just expression, but the token as well
            if currentState=TOKEN then
                //detected end of non-lambda token
                tokenEnd<-i
                let tokenStr=source.Substring(tokenStart, tokenEnd-tokenStart)
                //process token
                let token=processToken tokenStr
                tokens<-token::tokens
                tokenStart<- -1
                tokenEnd<- -1

            //end expr
            exprEnd<-i-1
            //process expression
            if 0<tokens.Length then 
                let expr=COMPOSITE(tokens|>List.rev,exprStart+offest,exprEnd+offest)
                expressions<-expr::expressions
                tokens<-[]
            exprStart<- -1
            exprEnd<- -1
            tokenStart<- -1
            tokenEnd<- -1
            ()
        |(SEPARATOR,WHITESPACE)->
            //start of new expr
            exprStart<-i
            ()
        |(SEPARATOR,TOKEN)->
            //start of new expr and new token
            exprStart<-i
            tokenStart<-i
            ()
        |(_,START)->
            panic("illegal transition")
        //move to new state and advance to new char
        currentState<-newState
        i<-i+1
    //process last pending token and expression
    if currentState=TOKEN then
        //detected end of non-lambda token
        tokenEnd<-i
        let tokenStr=source.Substring(tokenStart, tokenEnd-tokenStart)
        //process token
        let token=processToken tokenStr
        tokens<-token::tokens
    if 0<tokens.Length then 
        exprEnd<-i
        let expr=COMPOSITE(tokens|>List.rev,exprStart+offest,exprEnd+offest)
        expressions<-expr::expressions
    expressions|>List.rev
let tabs i=
    String.replicate i "  "
let printLiteral lit indent=
    match lit with
    |BOOL x->printfn "%sBOOL: %A" (tabs indent) x 
    |INT x->printfn "%sINT: %A" (tabs indent) x
    |FLOAT x->printfn "%sFLOAT: %A" (tabs indent) x
    |STR x->printfn "%sSTR: %A" (tabs indent) x
    |TIME x->printfn "%sTIME: %A" (tabs indent) x
let rec printAst (source:string) ast indent=
    ast|>Seq.iter(fun expr->
        match expr with
        |COMPOSITE(x,ib,ie)->
            printfn "%sCOMPOSITE: '%s' [" (tabs indent) (source.Substring(ib,ie-ib+1))
            printAst source x (indent+1)
            printfn "%s]" (tabs indent)
        |LITERAL x->printLiteral x indent
        |VAR x->printfn "%sVAR: %s" (tabs indent) x
        |VARIDX (x,i)->printfn "%sVARIDX: %s %d" (tabs indent) x i
        |VARMAP (x,k)->printfn "%sVARMAP: %s %s" (tabs indent) x k
        |DOTS j->printfn "%sDOTS: %d" (tabs indent) j
        |DOTSIDX (j,i)->printfn "%sDOTSIDX: %d %d" (tabs indent) j i
        |DOTSMAP (j,k)->printfn "%sDOTSMAP: %d %s" (tabs indent) j k
        |LAM(par,expr)->
            printfn "%sLAM; params: %A" (tabs indent) par
            match expr with 
            |COMPOSITE (exprs,ib,ie)->printAst source exprs (indent+1)
            |_->panic("lambda's body can only be list of expressions")
    )
let interp source=
    let ast=createAst source 0
    printAst source ast 0

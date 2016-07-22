module config
//system types
type Console=System.Console
type Math=System.Math
type Type=System.Type
type File=System.IO.File
type Directory=System.IO.Directory
type Dictionary<'a,'b>=System.Collections.Generic.Dictionary<'a,'b>
type IList=System.Collections.IList
type DateTime=System.DateTime
//app types
type Literal=
    |BOOL of bool
    |INT of int
    |FLOAT of float
    |STR of string
    |TIME of DateTime
type Expr=
    //tokens - literal
    |LITERAL of Literal
    //tokens - vars
    |VAR of string
    |VARIDX of string*int
    |VARMAP of string*string
    |DOTS of int
    |DOTSIDX of int*int
    |DOTSMAP of int*string
    //tokens - lambda
    |LAM of string list*Expr
    //expression - list of tokens and start/end indexes
    |COMPOSITE of Expr list*int*int
//global vars:
//bindings
let mutable SCOPES:List<Dictionary<string,Expr>>=[]//we will be moving up/down this list; keep scopes in place, just reset them to speed things up (up to max # of scopes)
let HELP="
\exit            exit shell
\hist            show commands history
"
let mutable HIST=[""]
let mutable DONE=false
let culture=System.Globalization.CultureInfo.InvariantCulture

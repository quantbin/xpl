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
let (|Suffix|_|) (p:string) (s:string) =
    if s.EndsWith(p) then
        Some(s.Substring(0,s.Length-p.Length))
    else
        None
let (|InBraces|_|) (s:string) =
    if s.StartsWith("{") && s.EndsWith("}") then
        Some(s.Substring(0,s.Length-1))
    else
        None
let concat t=t|>String.concat " "
let panic s=raise(new System.Exception(s))

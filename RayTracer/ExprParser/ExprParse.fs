module ExprParse

(* Grammar:

E    = T Eopt .
Eopt = "+" T Eopt | e .
T    = F Topt .
Topt = "*" F Topt | e .
F    = P Fopt .
Fopt = "^" Int | e .
P    = Int [ Float | Var | "(" E ")" .

e is the empty sequence.
*)

type terminal =
  Add | Sub | Mul | Div | Pwr | Root | Lpar | Rpar | Int of int | Float of float | Var of string

let isblank c = System.Char.IsWhiteSpace c
let isdigit c  = System.Char.IsDigit c
let isletter c = System.Char.IsLetter c
let isletterdigit c = System.Char.IsLetterOrDigit c

let explode s = [for c in s -> c]

let floatval (c:char) = float((int)c - (int)'0')
let intval(c:char) = (int)c - (int)'0'

exception Scanerror

let rec scnum (cs, value) = 
  match cs with 
    '.' :: c :: cr when isdigit c -> scfrac(c :: cr, (float)value, 0.1)
  | c :: cr when isdigit c -> scnum(cr, 10* value + intval c)
  | _ -> (cs,Int value)    (* Number without fraction is an integer. *)
and scfrac (cs, value, wt) =
  match cs with
    c :: cr when isdigit c -> scfrac(cr, value+wt*floatval c, wt/10.0)
  | _ -> (cs, Float value)

let rec scname (cs, value) =
  match cs with
    c :: cr when isletterdigit c -> scname(cr, value + c.ToString())
  | _ -> (cs, Var value)

/// <summary>
/// Just eats whitespace characters and returns the char sequence at the first non-whitespace character.
/// </summary>
/// <param name="cs">char sequence to eat from</param>
let rec eatwhitespace cs =
    match cs with
      c :: cr when isblank c -> eatwhitespace cr
    | _ -> cs

let scan (s:char seq) =
  let rec sc cs = 
    match cs with
      [] -> []
    | '+' :: cr -> Add :: sc cr                    
    | '*' :: cr -> Mul :: sc cr  
    | '/' :: cr -> Div :: sc cr    
    | '^' :: cr -> Pwr :: sc cr
    | '_' :: cr -> Root :: sc cr
    | '(' :: cr -> Lpar :: sc cr     
    | ')' :: cr -> insertSub (cr, Rpar)
    | '-' :: cr -> Int -1 :: Mul :: sc cr
    | c :: cr when isblank c -> sc cr
    | c :: cr when isdigit c -> scnum(cr, intval c) |> insertSub
    | c :: cr when isletter c -> scname(cr, (string)c) |> insertSub 
    | _ -> raise Scanerror
  and insertSub (s, before) = // Function for checking whether a Sub terminal should be inserted
    let cs1 = eatwhitespace s // remove possible whitespace before next character
    match cs1 with // Check if next character after before (a terminal) is a "-".
    | '-' :: cs2 -> before :: Sub :: sc cs2 // If it is, add a Sub token in between the terminal "before" and rest of the expression
    | _ -> before :: sc cs1 // Else just add the ")" and continue
  sc (explode s)



let (|Val|_|) x = 
    match x with
      Float _ | Int _ | Var _ -> Some x
    | _ -> None

let rec insertMult = function
  Val a :: Val b :: ts -> a :: Mul :: insertMult(b::ts)
| Val a :: Lpar :: ts  -> a :: Mul :: Lpar :: insertMult(ts)
| Rpar :: Val b :: ts  -> Rpar :: Mul :: insertMult(b::ts)
| t :: ts -> t :: insertMult ts
| [] -> []
  
type expr = 
  | FNum of float
  | FVar of string
  | FAdd of expr * expr
  | FMult of expr * expr
  | FDiv of expr * expr
  | FExponent of expr * int
  | FRoot of expr * int

exception Parseerror

let rec E (ts:terminal list) = (T >> Eopt) ts
and Eopt (ts, inval) =
  match ts with
    Add :: tr -> let (ts1, tv) = T tr
                 Eopt (ts1, FAdd(inval, tv))
  | Sub :: tr -> let (ts1, tv) = T tr
                 Eopt (ts1, FAdd(inval, FMult(FNum -1., tv)))
  | _ -> (ts, inval)
and T ts = (F >> Topt) ts
and Topt (ts, inval) =
  match ts with
    Mul :: tr -> let (ts1, tv) = F tr in Topt (ts1, FMult(inval, tv))
  | Div :: tr -> let (ts1, tv) = F tr in Topt (ts1, FDiv(inval, tv))
  | _ -> (ts, inval)
and F ts = (P >> Fopt) ts
and Fopt (ts, inval) = 
  match ts with
    Pwr :: Int(x) :: tr -> (tr, FExponent(inval, x))
  | Root :: Int(x) :: tr -> (tr, FRoot(inval, x))
  | _ -> (ts, inval)
and P ts = 
  match ts with
    Int x :: tr   -> (tr, FNum(float(x)))
  | Float x :: tr -> (tr, FNum(x))
  | Var n :: tr   -> (tr, FVar(n))
  | Lpar :: tr    -> let (ts1, v) = E tr
                     match ts1 with
                       Rpar :: tr -> (tr, v)
                     | _ -> raise Parseerror
  | _ -> raise Parseerror


let parse ts = 
  match E ts with
    ([], result) -> result
  | _ -> raise Parseerror

let parseStr s = (scan >> insertMult >> parse) s

let dotAST ast =
  let fixStr (s:string) = s.Replace ("\"", "\\\"")
  let genDot s n e = "digraph G {\nlabel=\"" + (fixStr s) + "\"\n" + n + e + "\n}"
  // i is unique label such that nodes and edges are unique in DiGraph.
  let genNodeStr i l = "Node"+(string i)+" [label=\""+l+"\"];\n"
  let genEdgeStr i1 i2 = "Node"+(string i1)+" -> " + "Node"+(string i2)+";\n"
  // Edges are unique and stored in a set.
  // Nodes are not unique and stored in a map, i.e., node with "+" may happen several times. 
  let rec genNE (i,nmap,eset) = function
    FNum r -> (i,Map.add i (genNodeStr i ((string)r)) nmap,eset)            // Add node with number
  | FVar x -> (i,Map.add i (genNodeStr i x) nmap,eset)                      // Add node with variable
  | FAdd (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1         // Generate nodes and edges for e1 and e2
                    let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                    (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "+") nmap2,                      // Add node for "+"
                     Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "+"->e1 and "+"->e2
  | FMult (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1        // Generate nodes and edges for e1 and e2
                     let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                     (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "*") nmap2,                      // Add node for "*"
                      Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "*"->e1 and "*"->e2
  | FExponent (e1,ie) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1                                // Generate nodes and edges for e1
                         let (i2,nmap2) = (i1+1,Map.add (i1+1) (genNodeStr (i1+1) ((string)ie)) nmap1)  // Add node for integer (exponent)
                         (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "^") nmap2,                            // Add node for "^"
                          Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset1))        // Add edges for "^"->e1 and "^"->ie
  let (_,nmap,eset) = genNE (0,Map.empty,Set.empty) ast  // Generate map for nodes and set for edges
  genDot (sprintf "%A\n" ast) (Map.fold (fun acc _ s -> acc + s) "" nmap) (Set.fold (fun acc s -> acc + s) "" eset)  // Generate big string with dot-code.
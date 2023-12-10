open System
open System.Numerics
open System.IO

type id = string
type expr =
| App of expr*expr
| Lam of id*expr
| Var of id
| Int of int
| String of string
| None
| PFunc of id
| Cond of expr*expr*expr
| Let of id*expr*expr
| LetRec of id*expr*expr
| Op of id*int*expr list
| Closure of expr*env
| RClosure of expr*env*id
and
  env = Map<id,expr>

let funof = function
| "+" -> (function [Int(a);Int(b)] -> Int(a+b))
| "-" -> (function [Int(a);Int(b)] -> Int(a-b))
| "*" -> (function [Int(a);Int(b)] -> Int(a*b))
| "/" -> (function [Int(a);Int(b)] -> Int(a/b))
| "=" -> (function [Int(a);Int(b)] -> if a=b then Int(1) else Int(0))
| ">" -> (function [Int(a);Int(b)] -> if a>b then Int(1) else Int(0))
| ">=" -> (function [Int(a);Int(b)] -> if a>=b then Int(1) else Int(0))
| "<" -> (function [Int(a);Int(b)] -> if a<b then Int(1) else Int(0))
| "<=" -> (function [Int(a);Int(b)] -> if a<=b then Int(1) else Int(0))
| "print" -> (function [expr.String(a)] -> printfn "%s" a; None)
| "printint" -> (function [expr.Int(a)] -> printfn "%d" a; None)
| _ -> (function [] -> None)

let funpars = function
| "+" | "-" | "*" | "/" | "=" | ">" | "<" | "<=" | ">=" -> 2
| "print" | "printint" | "func" -> 1

let predefFuncs = [
    "+";
    "-";
    "*";
    "/";
    "=";
    ">";
    ">";
    "<";
    "<=";
    "print";
    "printint";
    "func";
]

let rec eval exp env =
    match exp with
    | Int(n) -> Int(n)
    | expr.String(n) -> expr.String(n)
    | None -> None
    | Var(x) -> Map.find x env
    | Lam(id,ex) -> Closure(exp,env)
    | App(ex1,ex2) -> apply (eval ex1 env) (eval ex2 env)
    | PFunc(id) -> Op(id,(funpars id),[])
    | Cond(e0,e1,e2) ->
        match eval e0 env with
            | Int(0) -> eval e2 env
            | expr.String("") | None -> eval e2 env
            | _ -> eval e1 env
    | Let(id,e1,e2) ->
        let e1' = eval e1 env in eval e2 (Map.add id e1' env)
    | LetRec(id,e1,e2) ->
        let e1' = RClosure(e1,env,id) in eval e2 (Map.add id e1' env)   
and apply e1 e2 =
    match e1 with
    | Closure(Lam(v,e),env) -> eval e (Map.add v e2 env)
    | RClosure(Lam(v,e),env,id) -> eval e (Map.add v e2 (Map.add id e1 env))
    | Op(id,n,args) ->
        if n=1 then (funof id)(e2::args)
        else Op(id,n-1,e2::args)

eval (App(App(PFunc("+"),Int(1)),Int(2))) Map.empty

eval (App(Lam("x",Var("x")),Int(5))) Map.empty

eval (App(Lam("x",App(App(PFunc("*"),Var("x")),Var("x"))),Int(5))) Map.empty

let example = 
    App(
        PFunc("printint"),
        LetRec("fact",
            Lam("x",
                Cond(App(App(PFunc("<="),Var("x")),Int(1)),
                Var("x"),
                App(App(PFunc("*"),Var("x")),
                    App(Var("fact"),App(App(PFunc("-"),Var("x")),Int(1)))))),
            App(Var("fact"),Int(5))))

eval example Map.empty

type Token =
    | String of string
    | Action of string
    | Number of string
    | OpenBracket | CloseBracket
    | OpenSection | CloseSection
    | Comma

let tokenize text = 
    let tokenEnder = function
        | ')' -> true
        | '(' -> true
        | h when Char.IsWhiteSpace(h) -> true
        | _ -> false

    let rec action' acc = function
        | [] -> acc, []
        | h :: t when tokenEnder h -> acc, (h::t)
        | h :: t -> action' (acc+h.ToString()) t

    let rec number' acc = function
        | [] -> acc, []
        | h :: t when Char.IsDigit(h) -> action' (acc+h.ToString()) t
        | h :: t -> acc, (h::t)

    let rec string' acc = function
        | [] -> acc, []
        | '\"' :: t -> acc, t
        | x :: t when Char.IsWhiteSpace(x) -> string' acc t
        | x :: t -> string' (acc+x.ToString()) t
    
    let rec tokenize' acc = function
        | [] -> Seq.rev acc
        | '(' :: t -> tokenize' (OpenBracket::acc) t
        | ')' :: t -> tokenize' (CloseBracket::acc) t
        | '{' :: t -> tokenize' (OpenSection::acc) t
        | '}' :: t -> tokenize' (CloseSection::acc) t
        | ',' :: t -> tokenize' (Comma::acc) t
        | h :: t when Char.IsWhiteSpace(h) -> tokenize' acc t
        | '\"' :: t -> 
            let token, rem = string' "" t
            tokenize' (Token.String(token) :: acc) rem
        | h :: t when Char.IsDigit(h) -> 
            let token, rem = number' (h.ToString())  t
            tokenize' (Token.Number(token)::acc) rem
        | h :: t ->
            let token, rem = action' (h.ToString()) t
            tokenize' (Token.Action(token) :: acc) rem 
            
    tokenize' [] text

let parse tokens = 
    let rec parseSection = function
        | CloseSection :: tail ->
            None, tail
        
        | CloseBracket :: tail ->
            None, tail
        
        | Action(iffunc) :: OpenBracket :: tail  when iffunc = "if" ->
            let condApp, rem = parseSection tail
            let trueApp, remNoTrue = parseSection (List.tail rem)
            let falseApp, remNoIf = parseSection (List.tail (List.tail remNoTrue))
            
            Cond(condApp, trueApp, falseApp), remNoIf

        | Action(func) :: Action(name) :: OpenBracket :: Action(x) :: CloseBracket :: OpenSection :: tail when func = "func" ->
            let lambda, afterSection = parseSection tail
            let other, rem = parseSection (List.tail afterSection)

            LetRec(name, Lam(x, lambda), other), rem
       
        | Action(func) :: OpenBracket ::  tail ->
            let x, rem = parseSection tail
            let app, other = parseSection rem
            let var = Var(func)
            match app with
                | None -> App(var, x), other
                | _ -> App(var, x), other // TODO

        | Action(func) :: tail when List.contains func predefFuncs-> 
            let app, rem = parseSection tail
            App(PFunc(func), app), rem

        | Action(func) :: tail ->
            let app, rem = parseSection tail
            let var = Var(func)
            match app with
                | None -> var, rem
                | _ -> App(app, var), rem
        
        | Number(s) :: tail ->
            let app, rem = parseSection tail
            let number = Int(s |> int)
            match app with
                | None -> number, rem
                | _ -> App(app, number), rem
        
        | Token.String(s) :: tail ->  
            let app, rem = parseSection tail
            let str = expr.String(s)
            match app with
                | None -> str, rem
                | _ -> App(app, str), rem

        | a :: tail -> 
            printfn "%A" a
            None, tail

        | [] -> 
            None, []

    let res, o = parseSection tokens
    res



let text = System.IO.File.ReadAllText("factorial.fgo")
let tokens = tokenize (text |> Seq.toList)
tokens
let parsed = parse (tokens |> Seq.toList)

parsed

eval parsed Map.empty

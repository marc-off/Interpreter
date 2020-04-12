(* AMBIENTE: INTERFACCIA *)

module type ENV =
    sig
        type 't env
        val emptyenv : 't -> 't env
        val bind : 't env * string * 't -> 't env
        val bindlist : 't env * (string list) * ('t list) -> 't env
        val applyenv : 't env * string -> 't
        exception WrongBindlist
    end
;;
    

(* AMBIENTE: SEMANTICA *)

module Funenv: ENV =
    struct
        type 't env = string -> 't
        exception WrongBindlist
        let emptyenv(x) = function (y: string) -> x
            (* x: valore default *)
        let applyenv(x, y) = x y
        let bind(r, l, e) = function lu -> if lu = l then e else applyenv(r, lu)

        let rec bindlist(r, il, el) = match (il, el) with
            | ([], []) -> r
            | (i::il1, e::el1) -> bindlist (bind(r, i, e), il1, el1)
            | _ -> raise WrongBindlist
    end
;;
   

(* AMBIENTE: IMPLEMENTAZIONE *)

module Listenv: ENV =
    struct
        type 't env = (string * 't) list
        exception WrongBindlist
        let emptyenv(x) = [("", x)]
        let rec applyenv(x, y) = match x with
            | [(_, e)] -> e
            | (i1, e1) :: x1 -> if y = i1 then e1 else applyenv(x1, y)
            | [] -> failwith("wrong env")
        let bind(r, l, e) = (l, e) :: r
        let rec bindlist(r, il, el) = match (il, el) with
            | ([], []) -> r
            | (i::il1, e::el1) -> bindlist (bind(r, i, e), il1, el1)
            | _ -> raise WrongBindlist
    end
;;
    

(* Tipi per generare l'ALBERO DI SINTASSI ASTRATTA *)

type ide = string ;;


(* Elemento tipico del dizionario, ovvero una coppia chiave-valore *)

type elemdict = ide * int ;;


(* Il dizionario, ovvero una collezione di elementi definiti poc'anzi *)

type dictionary = elemdict list;;



(* LINGUAGGIO DIDATTICO FUNZIONALE *)
    
type exp =
    | CstInt of int
    | CstTrue
    | CstFalse
    | Times of exp * exp
    | Sum of exp * exp
    | Sub of exp * exp
    | Eq of exp * exp
    | Iszero of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    | Fun of ide * exp
    | Den of ide
    | Apply of exp * exp
    | Insert of elemdict * dictionary
    | Delete of ide * dictionary
    | Has_Key of ide * dictionary
    | Iterate of ( int -> int ) * dictionary
    | Fold of ( int  -> int ) * dictionary
    | Filter of (ide list) * dictionary
;;


type efun = ide * exp ;;


type evT =
    | Int of int
    | Bool of bool
    | Unbound
    | Closure of ide * exp * evT Listenv.env
    | Funval of efun
    | Dictionary of dictionary
;;


(* CONTROLLO DEI TIPI *)

let typecheck (x, y) = match x with
    | "int" -> (match y with
                    | Int(u) -> true
                    | _ -> false)
    | "bool" -> (match y with
                    | Bool(u) -> true
                    | _ -> false)
    | "dictionary" -> (match y with
                    | Dictionary(u) -> true
                    | _ -> false)
    | _ -> failwith ("not a valid type")
;;


(* OPERAZIONI BASE (valutate secondo una strategia eager) *)

let is_zero x = match (typecheck ("int",x), x) with
    | (true, Int(y)) -> Bool(y=0)
    | (_, _) -> failwith("run-time error")
;;


let int_eq(x,y) = match (typecheck ("int",x), typecheck ("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Bool(v = w)
    | (_,_,_,_) -> failwith("run-time error ")
;;


let int_times(x, y) = match (typecheck ("int",x), typecheck ("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v * w)
    | (_,_,_,_) -> failwith("run-time error ")
;;


let int_plus(x, y) = match (typecheck ("int",x), typecheck ("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v + w)
    | (_,_,_,_) -> failwith("run-time error ")
;;


let int_sub(x, y) = match (typecheck ("int",x), typecheck ("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v - w)
    | (_,_,_,_) -> failwith("run-time error ")
;;


let bool_and(x, y) = match (typecheck ("bool",x), typecheck ("bool",y), x, y) with
    | (true, true, Bool(v), Bool(w)) -> Bool(v && w)
    | (_,_,_,_) -> failwith("run-time error ")
;;


let bool_or(x, y) = match (typecheck ("bool",x), typecheck ("bool",y), x, y) with
    | (true, true, Bool(v), Bool(w)) -> Bool(v || w)
    | (_,_,_,_) -> failwith("run-time error ")
;;


let bool_not(x) = match (typecheck ("bool",x), x) with
    | (true, Bool(v)) -> Bool(not v)
    | (_,_) -> failwith("run-time error ")
;;


 (* FUNZIONE AUSILIARIA PER LA RIMOZIONE DI OCCORRENZE DI ELEMENTI IN UN DIZIONARIO *)

 let rec remove_dictionary ( x : ide ) ( y : dictionary ) : dictionary =
     match y with
         | [] -> []
         | (key, value)::rest -> if (x=key) then (remove_dictionary x rest) else (key,value)::(remove_dictionary x rest)
 ;;
         

 (* FUNZIONE AUSILIARIA PER LA RICERCA DI OCCORRENZE DI ELEMENTI IN UN DIZIONARIO *)

 let rec occurrence_dictionary ( x : ide ) ( y : dictionary ) : bool =
     match y with
         | [] -> false
         | (key, value)::rest -> if (x=key) then true else occurrence_dictionary x rest
 ;;
         
         
 (* FUNZIONE AUSILIARIA PER APPLICAZIONE DI FUNZIONI AI VALORI PRESENTI IN CIASCUNA COPPIA IN UN DIZIONARIO *)

 let rec apply_dictionary (func : int -> int ) (y : dictionary) : dictionary =
     match y with
         | [] -> []
         | (key, value)::rest -> ( key, (func value) )::( apply_dictionary func rest )
 ;;
         

 (* FUNZIONE AUSILIARIA PER APPLICAZIONE IN STILE "FOLD" AI VALORI PRESENTI IN CIASCUNA COPPIA IN UN DIZIONARIO *)

 let rec fold_dictionary (func : int -> int) (y : dictionary) : int =
     match y with
         | [] -> 0
         | (key, value)::rest -> (func value) + (fold_dictionary func rest)
 ;;


 (* FUNZIONE AUSILIARIA PER FILTRAGGIO DEL DIZIONARIO IN BASE AD UNA LISTA DI CHIAVI IDENTIFICATIVE *)

 let rec filter_dictionary (x : ide list) (y : dictionary) : dictionary =
     match y with
         | [] -> []
         | (key, value)::rest ->
             let rec contained (a : ide) (b : ide list) : bool =
                 match b with
                     | [] -> false
                     | id::xs -> if (a=id) then true else contained a xs
             in
                 if (contained key x) then (key, value)::(filter_dictionary x rest) else filter_dictionary x rest
 ;;


(* CICLO INTERPRETE *)

let rec eval (e: exp) (amb: evT Listenv.env) : evT =
    match e with
        | CstInt(n) -> Int(n)
        | CstTrue -> Bool(true)
        | CstFalse -> Bool(false)
        | Iszero(e1) -> is_zero(eval e1 amb)
        | Eq(e1, e2) -> int_eq((eval e1 amb), (eval e2 amb))
        | Times(e1,e2) -> int_times((eval e1 amb), (eval e2 amb))
        | Sum(e1, e2) -> int_plus ((eval e1 amb), (eval e2 amb))
        | Sub(e1, e2) -> int_sub ((eval e1 amb), (eval e2 amb))
        | And(e1, e2) -> bool_and((eval e1 amb), (eval e2 amb))
        | Or(e1, e2) -> bool_or ((eval e1 amb), (eval e2 amb))
        | Not(e1) -> bool_not((eval e1 amb))
        | Ifthenelse(cond,e1,e2) -> let g = eval cond amb in
                                        (
                                        match (typecheck("bool", g), g) with
                                            | (true, Bool(true)) -> eval e1 amb
                                            | (true, Bool(false)) -> eval e2 amb
                                            | (_, _) -> failwith ("nonboolean guard")
                                        )
        | Let(i, e1, ebody) -> eval ebody (Listenv.bind (amb, i, (eval e1 amb)))
        | Fun(i, a) -> Closure(i, a, amb)
        | Den(func) -> Listenv.applyenv (amb, func)
        | Apply(Den(f), eArg) -> let fclosure = Listenv.applyenv (amb, f) in
                                    (
                                    match fclosure with
                                        | Closure(arg, fbody, fDecEnv) -> let aVal = eval eArg amb in
                                                                            let aenv = Listenv.bind (fDecEnv, arg, aVal) in
                                                                                eval fbody aenv
                                        | _ -> failwith("non functional value")
                                    )
        | Apply(_,_) -> failwith ("Application: not first order function")
        | Insert (elem, dict) -> Dictionary( elem :: dict )
        | Delete (id, dict) -> Dictionary( remove_dictionary id dict )
        | Has_Key (id, dict) -> Bool( occurrence_dictionary id dict )
        | Iterate (func, dict) -> Dictionary( apply_dictionary func dict )
        | Fold (func, dict) -> Int( fold_dictionary func dict )
        | Filter (idlist, dict) -> Dictionary( filter_dictionary idlist dict )
;;



(* FUNZIONE CHE PRENDE UN PARAMETRO DI TIPO UNIT E RESTITUISCE UN DIZIONARIO VUOTO *)

let default_dict (x : unit) : dictionary = [("mele",34)];;

(* CREIAMO UN DIZIONARIO DI DEFAULT DI TIPO ENV *)

let x = Dictionary (default_dict ());;

(* CREIAMO UNA FUNZIONE CHE RESTITUISCE IL CONTENUTO DEL TIPO ENV CASO DIZIONARIO *)

let view_content (x : evT) : dictionary =
    match x with
        | Dictionary(lst) -> lst
        | _ -> failwith ("Case DICTIONARY expected!")
;;

(* ESEGUIAMO ALCUNI TEST CON IL DIZIONARIO DI DEFAULT "X" *)

let op1 = Insert (("pere", 45), view_content x);;

let res1 = eval op1 (Listenv.emptyenv (Unbound));;

let x = match (typecheck("dictionary", res1), res1) with
    | (true, Dictionary(u)) -> Dictionary(u)
    | _ -> failwith ("non dictionary value")
;;

let op2 = Insert (("cipolle", 56), view_content x);;

let res2 = eval op2 (Listenv.emptyenv (Unbound));;

let x = match (typecheck("dictionary", res2), res2) with
    | (true, Dictionary(u)) -> Dictionary(u)
    | _ -> failwith ("non dictionary value")
;;

let op3 = Insert (("patate", 132), view_content x);;

let res3 = eval op3 (Listenv.emptyenv (Unbound));;

let x = match (typecheck("dictionary", res3), res3) with
    | (true, Dictionary(u)) -> Dictionary(u)
    | _ -> failwith ("non dictionary value")
;;

(* TEST DI DEFAULT *)

let x_dlt = eval (Delete(("mele"), view_content x)) (Listenv.emptyenv (Unbound));;

eval (Has_Key("mele", view_content x)) (Listenv.emptyenv (Unbound));;

let x = eval (Iterate((fun x -> x + 1), view_content x)) (Listenv.emptyenv (Unbound));;

let x_res = eval (Fold((fun x -> x + 2), view_content x)) (Listenv.emptyenv (Unbound));;

let x_filtr = eval (Filter((["mele";]), view_content x)) (Listenv.emptyenv (Unbound));;

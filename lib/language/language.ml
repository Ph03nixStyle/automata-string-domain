open Ast;;


let log_states (a: 'a Automates.t) : unit =
  let f = open_out_gen [Open_append; Open_creat] 0o666 "test.txt"  in
  Printf.fprintf f "%s\n" (string_of_int a.nb)
;;

let rec read_cond (c: cond) (sigma: char array) : char Automates.t =
  match c with
  | Unknown -> failwith "should not happen"
  | ConstBool(b) -> if b then Automates_builder.sigma_etoile sigma else Automates_builder.vide sigma
  | ContainsChar(lettre) -> Automates_builder.contient_lettre sigma lettre
  | And(c1, c2) -> Automates.intersection (read_cond c1 sigma) (read_cond c2 sigma)
  | Or(c1, c2) -> Automates.union (read_cond c1 sigma) (read_cond c2 sigma)
  | Not(c1) -> Automates.complementaire (read_cond c1 sigma)
;;


let read_stringop (op: stringop) (a: char Automates.t) (collect: bool) =
  match op with
  | AssignGlobalString(str) -> begin
    if collect then Automates_builder.sigma_etoile a.sigma else (*While n'est pas implémenté pour AssignGlobalString*)
    match str with
    | StringLiteral(s) -> Automates_builder.from_str a.sigma s
    | UserInput -> Automates_builder.sigma_etoile a.sigma
  end
  | PushLeft(str) -> begin
    if collect then Automates_builder.sigma_etoile a.sigma else (*While n'est pas implémenté pour PushLeft*)
    match str with
    | StringLiteral(s) -> Automates.concatenation (Automates_builder.from_str a.sigma s) a
    | UserInput -> Automates.concatenation (Automates_builder.sigma_etoile a.sigma) a
  end
  | PushRight(str) -> begin
    match str with
    | StringLiteral(s) -> Automates.concatenation a (Automates_builder.from_str a.sigma s)
    | UserInput -> Automates.concatenation a (Automates_builder.sigma_etoile a.sigma)
  end
  
;;

let rec read_stat (s: stat) (a: char Automates.t)  (collect: bool) : char Automates.t =
  match s with
  | LogStates -> (log_states a; a)
  | Minimize -> (Automates.minimal_brzozowski a)
  | Seq(s1) -> begin
    match s1 with
    | [] -> a
    | h::t -> let auto_tmp = read_stat h a collect in read_stat (Seq(t)) auto_tmp collect
  end
  | Skip -> a
  | Assert(c) -> if c = Unknown || collect then a else Automates.intersection a (read_cond c a.sigma)
  | StringOp(op) -> read_stringop op a collect
  | If(c, s1, s2) -> begin
    if c = Unknown then Automates.union (read_stat s1 a collect) (read_stat s2 a collect) else    
    let a_cond = read_cond c a.sigma in
    let auto_if = Automates.intersection a a_cond in (*Automate d'entrée du if*)
    let auto_else = Automates.intersection a (Automates.complementaire a_cond) in (*Automate d'entrée du else*)
    Automates.union (read_stat s1 auto_if collect) (read_stat s2 auto_else collect) (*Automate de sortie après le if ou le else*)
  end
  | While(_c, s1) ->
    let a2 = read_stat s1 (Automates_builder.epsilon a.sigma) true in Automates.etoile_push_right a a2
;;

let read_prog (p: prog) : char Automates.t =
  let a = read_stat p.code (Automates_builder.epsilon p.sigma) false in
  a
;;
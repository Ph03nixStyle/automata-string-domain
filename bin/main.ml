open! Automates;;
(* open Language;;
open Ast;; *)

(*(abandon de l'idée for now) ça peut être marrant une "réciproque" pour passer des automates aux programmes nan ? ça permettrait directement de faire un programme safe by design.*)


(*TODO autre algo minimisation si le temps*)
(*TODO faire graphique d'évolution de perf + de nb d'états. Faire générateur de progarmmes *)


(* let generate_if (n: int) : Ast.stat = (*TODO vite continuer ça*)
  let rec aux (acc: Ast.stat list) n =
    if n > 0 then aux ((If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1")))))::acc) (n-1)
    else acc
  in
  let ifs = aux [] n in
  Seq(StringOp(AssignGlobalString(StringLiteral""))::ifs) *)
;;

(* let time f x =
  let start = Unix.gettimeofday ()
  in let res = f x
  in let stop = Unix.gettimeofday ()
  in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
  in
     res *)



let () =
  let _sigma = [|'0'; '1'|] in
  let _code: Ast.stat =
    Seq([
      StringOp(AssignGlobalString(StringLiteral""));
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      LogStates;
      Minimize;
      LogStates;      
    ]) in
  let _code1: Ast.stat = 
    Seq([
      StringOp(AssignGlobalString(StringLiteral""));
      While(Unknown,
        Seq([
          If(Unknown,
            StringOp(PushRight(StringLiteral("01"))),
            StringOp(PushRight(StringLiteral("100")))
          );
          StringOp(PushRight(StringLiteral("1")))
        ])
      )
    ]) in
  let _code2: Ast.stat = 
    Seq([
      StringOp(AssignGlobalString(StringLiteral("")));
      If(Unknown, StringOp(PushRight(StringLiteral("1"))), StringOp(PushLeft(StringLiteral("0"))));
      StringOp(PushLeft(StringLiteral("1")));
    ])
  in

  let _prog: Ast.prog = {code = _code; sigma = _sigma} in
  let _prog1: Ast.prog = {code = _code1; sigma = _sigma} in
  let _prog2: Ast.prog = {code = _code2; sigma = _sigma} in

  let _res = Language.read_prog _prog in
  let _res1 = Language.read_prog _prog1 in
  let _res2 = Language.read_prog _prog2 in

  let _res3 = Automates.union _res1 _res2 in
  let _res4 = Automates.concatenation _res1 (Automates_builder.epsilon _sigma) in
  let _res5 = Automates.etoile_push_right (Automates_builder.epsilon _sigma) _res1 in
  let _res6 = Automates.union _res4 _res5 in

  Automates.generate_graphviz_file _res "test";
  (* Automates.affiche_char _res; *)
  (* Automates.affiche_char (determinise _res); *)
  (* Automates.affiche_char (transpose (determinise _res)); *)
  (* Automates.generate_graphviz_file _res "test_determinise"; *)
  (* Automates.generate_graphviz_file (transpose _res) "test_determinise"; *)
  (* Automates.generate_graphviz_file (determinise_non_renomme (transpose _res)) "test_determinise"; *)
  (* Automates.generate_graphviz_file (transpose (determinise (transpose _res))) "test_determinise"; *)
  (* Automates.generate_graphviz_file (determinise (transpose (determinise (transpose _res)))) "test_determinise"; *)

  (* Automates.generate_graphviz_file (minimal_brzozowski _res) "test_minimal"; *)
;;

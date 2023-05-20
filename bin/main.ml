(* open Automates;;
open Language;;
open Ast;; *)

(*TODO ça peut être marrant une "réciproque" pour passer des automates aux programmes nan ? :D ça permettrait directement de faire un programme safe by design.*)

(*TODO
   - minimal_brzozowski
   - minimal_autre
   - 2^n illustrer la gravité du truc (illustrations/faire le code/générer un nombre arbitraire de lignes "if" pour courbe évolutive)
   - faire plusieurs ast (chacun dans un exécutable, ou alors tout ici), avec un fichier associé pour le programme qu'il représente.
    chacun devra créer un fichier .dot qu'on visualise à part.*)

let () =
  let _sigma = [|'0'; '1'|] in
  let _code: Ast.stat =
    Seq([
      StringOp(AssignGlobalString(StringLiteral""));
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
      If(Unknown, StringOp(PushRight(StringLiteral("0"))), StringOp(PushRight(StringLiteral("1"))));
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

  Automates.generate_graphviz_file _res "test0";
  Automates.generate_graphviz_file (Automates.determinise _res) "test0";
  (* Automates.generate_graphviz_file (Automates.minimal_brzozowski _res) "test0"; *)
;;

(*new_string(str) -> automate avec chaque lettre de str
concat(s1, s2) -> concaténation de langages. 2 implémentations possibles:
    - soit on copie colle l'automate sur chacun des états de fin, puis je calcule l'automate minimal
    - soit on crée un état commun de fin avec des E-transitions, puis on déterminise. Important de déterminiser?? bonne question. On peut aussi calculer automate minimal.
readline() -> automate avec 1 état qui boucle sur lui-même, bref qui reconnait tout
substring[idx_debut, idx_fin](str) -> renvoie l'ensemble des chemins possibles qui commencent à l'idx début et finissent à l'idx fin, puis on fait l'union -> DFS un peu complexe lol
    (on applique automate minimal dessus, raison de pourquoi: on trouve un exemple qui fait exploser l'automate créé). A AMELIORER
contains[c](str) -> faut tester si tous les chemins obligent à prendre la lettre c -> doux jésus.*)

(*Une seule variable est considérée dans un programme: GlobalString. Les autres strings*)

(*un objet de type str ne représente jamais le string global d'étude, qui est non-représentable.*)
type str =
| StringLiteral of string  (*String donné directement par un litéral*)
| UserInput (*Peut être n'importe quel string *)

type idx = int

type cond =
  | Unknown
  | ConstBool of bool
  | ContainsChar of char
  | And of cond * cond
  | Or of cond * cond
  | Not of cond

type stringop =
  | AssignGlobalString of str (*initialisation/reset du string global*)
  | PushRight of str (*var <- concat(str, str) *)
  | PushLeft of str
  

type stat =
  | Seq of stat list       (*stat; stat; stat...*)
  | If of cond * stat * stat (*if cond then stat else stat done*)
  | Skip                     (*do nothing*)
  | Assert of cond           (*assert (cond)*)
  | StringOp of stringop
  | While of cond * stat     (*while cond do stat done *)

type prog = {code: stat; sigma: char array}
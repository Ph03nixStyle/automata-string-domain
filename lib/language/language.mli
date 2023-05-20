(** [read_prog p] génère un automate reconnaissant (au moins) l'ensemble des valeurs prises par le string global.*)
val read_prog : Ast.prog -> char Automates.t
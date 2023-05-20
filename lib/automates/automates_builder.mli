open Automates;;

(** [vide sigma] renvoie un automate reconnaissant le langage vide.*)
val vide : 'a array -> 'a t

(** [epsilon sigma] renvoie un automate reconnaissant le langage {epsilon}. (string vide)*)
val epsilon : char array -> char t

(** [sigma_etoile sigma] renvoie un automate reconnaissant le langage sigma étoile (langage complet).*)
val sigma_etoile : 'a array -> 'a t

(** [contient_lettre sigma c] renvoie un automate reconnaissant le language des mots contenant au moins 1 fois la lettre c*)
val contient_lettre : 'a array -> 'a -> 'a t

(** [lettre_nieme_pos sigma c idx] renvoie un automate reconnaissant le language des mots contenant lettre c en nième position*)
val lettre_nieme_pos : 'a array -> 'a -> int -> 'a t

val from_str : char array -> string -> char t

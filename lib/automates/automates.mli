
(** Représente un mot lu par un automate*)
type 'a mot_t = 'a list

(** Représente un état d'automate (un entier)*)
type etat_t = int

(** Représente un automate: delta est la fonction de transition, i et f les états initiaux et finaux, nb le nombre d'états, sigma l'alphabet.*)
type 'a t = {
  nb : etat_t;
  sigma : 'a array;
  i : etat_t list;
  f : etat_t list;
  delta : (etat_t * 'a, etat_t list) Hashtbl.t;
}

(** Exception levée lorsque l'on essaye d'ajouter une transition avec une lettre qui n'appartient pas à l'alphabet sigma de l'automate*)
exception LettreInexistante


(** [generate_graphviz_file a nom] crée un fichier graphviz [nom].dot représentant l'automate [a]*)
val generate_graphviz_file : char t -> string -> unit

(** [affiche a printer] Affiche les différentes composantes et transitions de l'automate d'entrée à l'aide de la fonction d'affichage [printer].*)
val affiche : 'a t -> ('a -> unit) -> unit

(** [affiche_char a] Affiche en termuinal un automate ayant un alphabet de type [char]*)
val affiche_char : char t -> unit

(** [renomme a] Ramène les états de l'automate sur un intervalle en enlevant les états vides*)
val renomme : 'a t -> 'a t

(** [ajoute_transition a q1 lettre q2] Ajoute la transition [(q1, lettre) -> q2] à l'automate [a]*)
val ajouter_transition : 'a t -> etat_t -> 'a -> etat_t -> unit

(** [retirer_trnsition a q1 lettre q2] Retire la transition [(q1, lettre) -> q2] à l'automate [a]. Raise Not_found si elle n'existe pas.*)
val retirer_transition : 'a t -> etat_t -> 'a -> etat_t -> unit

(** [est_deterministe a] Renvoie true si l'automate [a] est déterministe, i.e si il y a au plus 1 état initial et
    au plus 1 état d'arrivée dans les transitions par couple [(etat, lettre)], et faux sinon.*)
val est_deterministe : 'a t -> bool

(** [accepte_mot a w] Renvoie true si [a] accepte [w], faux sinon.*)
val accepte_mot : 'a t -> 'a mot_t -> bool

(** [determinise a] Renvoie une version déterministe de l'automate [a].*)
val determinise_non_renomme : 'a t -> 'a t
val determinise : 'a t -> 'a t

(** [union a1 a2] Renvoie un automate représentant l'union de [a1] et [a2]*)
val union : 'a t -> 'a t -> 'a t

(** [intersection a1 a2] Renvoie un automate représentant l'intersection de [a1] et [a2]*)
val intersection : 'a t -> 'a t -> 'a t

(** [complet a] Renvoie un automate représentant l'automate [a] complété*)
val complet : 'a t -> 'a t

(** [complementaire a] Renvoie un automate représentant le complémentaire de [a]**)
val complementaire : 'a t -> 'a t

(** [complementaire a] Renvoie l'automate transposé de [a] (états finaux/initiaux échangés, transitions retournées)**)
val transpose : 'a t -> 'a t

(** [complementaire a] Renvoie l'automate minimal équivalent à [a] par algorithme de Brzozowski**)
val minimal_brzozowski: 'a t -> 'a t

(** [concatenation a1 a2] Renvoie un automate représentant la concaténation de [a1] et [a2]*)
val concatenation : 'a t -> 'a t -> 'a t

(** [est_vide a] renvoie true si [a] est vide, false sinon.*)
val est_vide : 'a t -> bool

(** [est_inclus_dans a1 a2] renvoie true si [a1] est inclus dans [a2], false sinon.*)
val est_inclus_dans : 'a t -> 'a t -> bool

(** [etoile_push_right a1 a2] renvoie l'automate de L1(L2)* si on appelle L1 et L2 les langages de [a1] et [a2], *)
val etoile_push_right : 'a t -> 'a t -> 'a t
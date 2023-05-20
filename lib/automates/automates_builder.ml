open Automates;;

let vide (sigma: 'a array) : 'a t =
  {nb = 0; sigma = sigma; i = []; f = []; delta = Hashtbl.create 2}
;;

let epsilon (sigma: 'a array) : 'a t =
  {nb = 1; sigma = sigma; i = [0]; f = [0]; delta = Hashtbl.create 2}
;;

let sigma_etoile (sigma: 'a array) : 'a t =
  let delta = Hashtbl.create (Array.length sigma) in
  Array.iter (fun lettre -> Hashtbl.add delta (0, lettre) [0]) sigma;
  {nb = 1; sigma = sigma; i = [0]; f = [0]; delta = delta};;
;;


let contient_lettre (sigma: 'a array) (c: 'a) : 'a t =
  assert (Array.mem c sigma);
  let taille_sigma = Array.length sigma in
  let delta = Hashtbl.create (2*taille_sigma + 1) in
  for i = 0 to taille_sigma - 1 do
    Hashtbl.add delta (0, sigma.(i)) [0];
    Hashtbl.add delta (1, sigma.(i)) [1];
  done;
  Hashtbl.replace delta (0, c) [0; 1];

  {nb = 2; sigma = sigma; i = [0]; f = [1]; delta = delta}
;;

 
let lettre_nieme_pos (sigma: 'a array) (c: 'a) (idx: int) : 'a t =
  assert (Array.mem c sigma);
  let taille_sigma = Array.length sigma in
  let delta = Hashtbl.create (taille_sigma * (idx + 1)) in
  for i = 0 to taille_sigma - 1 do
    for k = 0 to idx - 1 do
      Hashtbl.add delta (k, sigma.(i)) [k+1];
    done;
    Hashtbl.add delta (idx+1, sigma.(i)) [idx+1];
  done;
  Hashtbl.add delta (idx, c) [idx+1];
  {nb = idx + 2; sigma = sigma; i = [0]; f = [idx + 1]; delta = delta}
;;

let from_str (sigma: char array) (s: string) : char t =
  let n = String.length s in
  let delta = Hashtbl.create n in
  for i = 0 to n-1 do
    if not (Array.mem s.[i] sigma) then raise LettreInexistante else
    Hashtbl.add delta (i, s.[i]) [i+1];
  done;

  {
    nb = n + 1;
    sigma = sigma;
    i = [0];
    f = [n];
    delta = delta;
  }
;;


let%test_unit _ =
  ()
  (* let sigma = [|'a'; 'b'|] in
  print_endline "Automate vide:";
  affiche_char (vide sigma);
  print_endline "Automate sigma_etoile: ------------------";
  affiche_char (sigma_etoile sigma);
  print_endline "Automate contient_lettre 'a': ------------------";
  affiche_char (contient_lettre sigma 'a');
  print_endline "Automate lettre_nieme_pos 'a' 5: ------------------";
  affiche_char (lettre_nieme_pos sigma 'a' 5);
  print_endline "Automate from_str 'abab': ------------------";
  affiche_char (from_str sigma "abab");
  print_endline "Automate from_str 'salut' avec alphabet {a,b}: ------------------";
  try affiche_char (from_str sigma "salut") with LettreInexistante -> print_endline "LettreInexistante raised (OK)"; *)
;;
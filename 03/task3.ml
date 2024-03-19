(* EXERCICE 1:  *)

type fin_partie =
  | Victoire
  | Nul
  | Défaite

let score_fin_partie fin =
  match fin with
  | Victoire -> 1.0
  | Nul -> 0.5
  | Défaite -> 0.0
;;

(* Tests *)
let score_victoire = score_fin_partie Victoire;;
let score_nul = score_fin_partie Nul;;
let score_defaite = score_fin_partie Défaite;;

(* On affiche les  résultats des tests  *)
Printf.printf "Score victoire : %.1f\n" score_victoire;;
Printf.printf "Score nul : %.1f\n" score_nul;;
Printf.printf "Score défaite : %.1f\n" score_defaite;;

(* EXERCICE 2:  *)


type etat = Metro | Boulot | Dodo

let transition_etat = function
  | Metro -> Boulot
  | Boulot -> Dodo
  | Dodo -> Metro;;

type jour_semaine =
  | Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

type semaine = jour_semaine

let est_un_weekend jour =
  match jour with
  | Samedi, true -> true
  | Dimanche, true -> true
  | _, false -> false;;


  type fin_de_partie = Victoire | Nul | Defaite

  let score_fin_de_partie fin =
    match fin with
    | Victoire -> 1.0
    | Nul -> 0.5
    | Defaite -> 0.0
    
(* EXERCICE 3:  *)

type fin_de_partie = Victoire | Nul | Defaite

let score_fin_de_partie fin =
  match fin with
  | Victoire -> 1.0
  | Nul -> 0.5
  | Defaite -> 0.0

type semaine = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

let est_un_weekend jour =
  match jour with
  | Samedi | Dimanche -> true
  | _ -> false

type etat = Metro | Boulot | Dodo

let transition_etat = function
  | Metro -> Boulot
  | Boulot -> Dodo
  | Dodo -> Metro;;

  (* Test *)
  let etat_courant = Metro in
  let etat_suivant = action_suivante etat_courant in
  Printf.printf "État actuel : %s, État suivant : %s\n"
    (match etat_courant with Metro -> "Metro" | Boulot -> "Boulot" | Dodo -> "Dodo")
    (match etat_suivant with Metro -> "Metro" | Boulot -> "Boulot" | Dodo -> "Dodo");;

    
(* EXERCICE 4:  *)

  let action_suivante etat_courant =
  match etat_courant with
  | Metro -> Boulot
  | Boulot -> Dodo
  | Dodo -> Metro;;

  (* Test *)
  let etat_courant = Metro in
  let etat_suivant = action_suivante etat_courant in
  Printf.printf "État actuel : %s, État suivant : %s\n"
    (match etat_courant with Metro -> "Metro" | Boulot -> "Boulot" | Dodo -> "Dodo")
    (match etat_suivant with Metro -> "Metro" | Boulot -> "Boulot" | Dodo -> "Dodo")
(* EXERCICE 5:  *)
type etat = Metro | Boulot | Dodo | Vacance;;

(* EXERCICE 6:  *)

type couleur = Pique | Coeur | Carreaux | Trefle

type valeur = As | Roi | Dame | Valet | Dix | Neuf | Huit | Sept

type carte_a_jouer = { valeur: valeur; couleur: couleur }

(* EXERCICE 7:  *)
let est_rouge carte =
  match carte.couleur with
  | Carreaux | Coeur -> true
  | _ -> false

let est_une_tete carte =
  match carte.valeur with
  | As | Roi | Dame | Valet -> true
  | _ -> false;;

(* EXERCICE 8:  *)

  let score_normal carte =
    match carte.valeur with
    | As -> 11
    | Dix -> 10
    | Roi -> 4
    | Dame -> 3
    | Valet -> 2
    | _ -> 0;;

  let score_atout carte =
    match carte.valeur with
    | As -> 11
    | Dix -> 10
    | Roi -> 4
    | Dame -> 3
    | Valet -> 20
    | _ -> 0;;

(* EXERCICE 9:  *)

let remporte_le_pli carte1 carte2 carte3 carte4 =
let meilleur_score carte =
  max (score_normal carte) (score_atout carte)
in
let score1 = meilleur_score carte1
and score2 = meilleur_score carte2
and score3 = meilleur_score carte3
and score4 = meilleur_score carte4 in
score1 >= score2 && score1 >= score3 && score1 >= score4;



(* Constructeurs avec données:  *)

(* EXERCICE 1 et 2  *)

type nombre =
  | Entier of int
  | Flottant of float;;

let somme x y =
  match x, y with
  | Entier a, Entier b -> Entier (a + b)
  | Entier a, Flottant b | Flottant b, Entier a -> Flottant (float_of_int a +. b)
  | Flottant a, Flottant b -> Flottant (a +. b);;

let difference x y =
  match x, y with
  | Entier a, Entier b -> Entier (a - b)
  | Entier a, Flottant b | Flottant b, Entier a -> Flottant 
 (float_of_int a -. b)
  | Flottant a, Flottant b -> Flottant (a -. b);;

let multiplication x y =
  match x, y with
  
  | Entier a, Entier b -> Entier (a * b)
  | Entier a, Flottant b | Flottant b, Entier a -> Flottant (float_of_int a *. b)
  | Flottant a, Flottant b -> Flottant (a *. b);;
  
let division x y =
  match x, y with
  | Entier a, Entier b -> Entier (a / b)
  | Entier a, Flottant b | Flottant b, Entier a -> Flottant (float_of_int a /. b)
  | Flottant a, Flottant b -> Flottant (a /. b);;
  
  (*   Types option et list*)

let division x y =
if y = 0 then
  None
else
  Some (x / y);;


  let salutation nom_opt =
  match nom_opt with
  | Some nom -> "Bonjour, " ^ nom
  | None -> "Bonjour, quel est ton nom ?";;


  let commence_par_un_trois liste =
  match liste with
  | [] -> false
  | x :: _ -> x = 3;;

  let premier_element liste =
  match liste with
  | [] -> None
  | x :: _ -> Some x;;




  
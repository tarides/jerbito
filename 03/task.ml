(* EXO 1  *)

type echec = Victoire | Nulle | Abondon

let echec_partie fin_partie = match fin_partie with 
| Victoire -> 1.
| Nulle -> 0.5
| Abondon -> 0.


(* EXO 2  *)
type semaine = Samedi | Dimanche | Lundi | Mardi | Mercredi | Jeudi | Vendredi;;

let est_un_weekend  jour = match jour with 
| Samedi | Dimanche -> true
| _ -> false


(* EXO 3  *)

type etat = Metro | Boulot | Dodo | Vacances

let transition_etat etat =
  match etat with
  | Metro -> Boulot
  | Boulot -> Dodo
  | Dodo -> Metro
  | Vacances -> Vacances

(* EXO 6 *)

type familles = Pique | Coeur | Carreaux | Trefle


type carte = As of familles 
| Sept of familles 
| Huit of familles 
| Neuf of familles 
| Dix of familles 
| Valet of familles 
| Dame of familles 
| Roi of familles 

type carte_a_jouer = { carte: carte; familles: familles}


(* EXO 7 *)

let est_une_tete c = match c with 
| Valet _ | Dame _| Roi _-> true
| _ -> false


let est_rouge c = match c with 
| Roi (Coeur | Carreaux) 
| Huit (Coeur | Carreaux) 
| Neuf (Coeur | Carreaux) 
| Dix (Coeur | Carreaux)  
| Dame (Coeur | Carreaux) 
| Sept (Coeur | Carreaux) 
| As (Coeur | Carreaux) 
| Valet (Coeur | Carreaux) -> true 
| _ -> false


let score_normal v = match v with 
| As (Pique | Coeur | Carreaux | Trefle) -> 13
| Dix (Pique | Coeur | Carreaux | Trefle) -> 10
| Roi (Pique | Coeur | Carreaux | Trefle) -> 4
| Dame (Pique | Coeur | Carreaux | Trefle) -> 3
| Valet (Pique | Coeur | Carreaux | Trefle) -> 2
| Neuf  (Pique | Coeur | Carreaux | Trefle) -> 0
| Huit  (Pique | Coeur | Carreaux | Trefle)-> 0
| Sept (Pique | Coeur | Carreaux | Trefle) -> 0

let score_atout v = match v with 
| As (Pique | Coeur | Carreaux | Trefle) -> 11
| Dix (Pique | Coeur | Carreaux | Trefle) -> 10
| Roi  (Pique | Coeur | Carreaux | Trefle)-> 4
| Dame (Pique | Coeur | Carreaux | Trefle) -> 3
| Valet (Pique | Coeur | Carreaux | Trefle) -> 20
| Neuf  (Pique | Coeur | Carreaux | Trefle)-> 14
| Huit (Pique | Coeur | Carreaux | Trefle)-> 0
| Sept (Pique | Coeur | Carreaux | Trefle) -> 0


let remporte_pli c1 c2 c3 c4 =
  if est_rouge(c1) then true else false
  




(* Constructeurs avec données *)

type types = Entier of int | Flottant of float


let somme a b =
  match (a, b) with
  | (Entier x,  Entier y) -> Entier(x + y)
  | (Flottant x,  Entier y) -> Flottant(x +. float_of_int(y))
  | (Entier x,  Flottant y) -> Flottant(float_of_int(x) +. y)
  | (Flottant x,  Flottant y) -> Flottant(x +. y)

let difference a b =
  match (a, b) with
  | (Entier x, Entier y) -> Entier (x - y)
  | (Flottant x,  Entier y) -> Flottant(x -. float_of_int(y))
  | (Entier x,  Flottant y) -> Flottant(float_of_int(x) -. y)
  | (Flottant x, Flottant y) -> Flottant (x -. y)

let multiplication a b =
  match (a, b) with
  | (Entier x, Entier y) -> Entier (x * y)
  | (Flottant x,  Entier y) -> Flottant(x *. float_of_int(y))
  | (Entier x,  Flottant y) -> Flottant(float_of_int(x) *. y)
  | (Flottant x, Flottant y) -> Flottant (x *. y)

let division a b =
  match (a, b) with
  | (Entier x, Entier y) -> if x mod y = 0 then Entier     (x / y) else Flottant (float_of_int(x) /. float_of_int(y))
  | (Flottant x,  Entier y) -> Flottant(x /. float_of_int(y))
  | (Entier x,  Flottant y) -> Flottant(float_of_int(x) /. y)
  | (Flottant x, Flottant y) -> Flottant (x /. y)


(* EXO 3 *)
type temperature = Celsius of float





(* Types enregistrements *)
(* EXO 1 *)
type point = {x : float; y : float}
type couleur = Pique | Coeur | Carreaux | Trefle
type force = A | K | Q | J  | Neuf | Huit | Sept | Dix
type carte = {couleur:couleur; force:force}
type carte_ou_Joker = Joker | Carte
let est_une_tete c = match c with {couleur = _ ; force = K | Q | J} -> true | _ -> false 

let compare_carte c1 c2 = 
let int_of_force x = match x with 
A -> 7
| K -> 6
| Q -> 5
| J -> 4
| Dix -> 3
| Neuf -> 2 
| Huit -> 1
| Sept -> 0
in 
if c1.couleur <> c2.couleur then (true: bool)
else int_of_force c1.force > int_of_force c2.force
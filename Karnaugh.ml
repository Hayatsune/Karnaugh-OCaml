(*Methode de Karnaugh*)


(*--------------------------------------------------*)
(* Autres fonctions *)

(*Calcule la puissance n de x.*)
let pow = fun x n ->
  let rec aux = fun n res ->
  if n=0
  then res
  else aux (n-1) (res*(abs x))
  in aux n 1;;

(*Calcule le nombre de chiffres qui composerait un decimal en binaire.*)
let nb_chiffre = function dec ->
  let rec aux = fun d res ->
    if (d/2)=0
    then res
    else aux (d/2) (res+1)
  in aux dec 1;;

(*Remplit un nombre binaire ou gray code "str" selon la place qu'occuperait le décimal "dec" dans une de ces representations.*)
let remplissage = fun str dec ->
  let rec aux = fun n res->
    if n<=0
    then res
    else aux (n-1) ("0"^res)
  in aux ((nb_chiffre dec)-(String.length str)) str;;


(*--------------------------------------------------*)
(* Fonctions sur les listes *)

(*Transforme une chaine de caracteres en liste de caracteres.*)
let list_of_string = fun str ->
  let lg = String.length str in
  let rec aux = fun c res ->
    if c<lg
    then aux (c+1) (res@[String.get str c])
    else res
  in aux 0 [];;

(*Calcule l'ensemble des elements contenus dans une liste.*)
let ensemble = fun liste ->
  let rec aux= fun l res -> match l with
      [] -> res
    | h::t -> if (List.mem h res)
      then aux t res
      else aux t (res@[h])
  in aux liste [];;

(*Indique si une liste l1 est incluse dans une liste l2.*)
let inclus = fun l1 l2 ->
  let rec aux = fun l -> match l with
    [] -> true
  | h::t -> if (List.mem h l2)
    then aux t
    else false
  in aux l1;;


(*--------------------------------------------------*)
(* Fonctions de conversion : Decimal - Binaire - Code Gray - String - Char *)

(*Conversion : Char->String*)
let string_of_char = function c -> String.make 1 c;;

(*Conversion : Decimal->Binaire*)
let bin_of_dec = function dec ->
  let rec aux = fun d res ->
    if (d<=0)
    then res
    else aux (d/2) ((string_of_int (d mod 2))^res)
  in if dec=0
    then "0"
    else aux dec "";;

(*Conversion : Binaire->GrayCode*)
let gray_of_bin = function bin ->
  let rec aux = fun b c res ->
    if (c=0)
    then res
    else begin
      if (String.get b 0 = String.get b 1)
      then aux (String.sub b 1 (String.length b - 1)) (c-1) (res^"0")
      else aux (String.sub b 1 (String.length b - 1)) (c-1) (res^"1")
    end
  in aux ("0"^bin) (String.length bin) "";;

(*Conversion : Decimal->GrayCode*)
let gray_of_dec = function dec ->
  gray_of_bin (bin_of_dec dec);;


(*--------------------------------------------------*)
(* Fonctions sur les formules *)

(*Indique si un caractere est une variable.*)
let est_variable = function var ->
  if (var<>' ')&&(var<>'+')&&(var<>'!')
  then true
  else false;;

(*Construit la liste des variables d'une formule.*)
let listVar = function formule ->
  List.filter est_variable (ensemble (list_of_string formule));;

(*Calcule le nombre de variable differente d'une formule.*)
let nb_variable = function formule ->
  List.length (listVar formule);;

(*Renvoie les variables sur les lignes et sur les colonnes.*)
let afficheVar = function formule ->
  let rec aux = fun cpt res ->
    if cpt >= (nb_variable formule)
    then res
    else if cpt = ((nb_variable formule / 2) - 1)
    then aux (cpt+1) (res^string_of_char (List.nth (listVar formule) cpt)^"\\")
    else aux (cpt+1) (res^string_of_char (List.nth (listVar formule) cpt))
  in aux 0 "";;

(*Separe les mots d'une formule dans une liste.*)
let listMot = function formule ->
  let listChar = list_of_string formule in
  let rec aux = fun lc mot res -> match lc with
      [] -> (res@[mot])
    | h::t -> if ((h<>' ') && (h<>'+'))
      then aux t (mot^(string_of_char h)) res
      else if (mot<>"")
      then aux t "" (res@[mot])
      else aux t "" res
  in aux listChar "" [];;

(*Transforme un mot d'une formule en nombre binaire.*)
let bin_of_mot = function mot ->
  let rec aux = fun m res ->
    if (String.length m=0)
    then res
    else match string_of_char(String.get m 0) with
	"" -> res
      | "!" -> aux (String.sub m 2 (String.length m - 2)) (res^"0")
      | _ -> aux (String.sub m 1 (String.length m - 1)) (res^"1")
  in aux mot "";;


(*--------------------------------------------------*)
(* Fonctions de construction de tableau de karnaugh. *)

(* Type : tabKarnaugh *)
(* - tab -> tableau des 0 et 1 sous forme de liste de listes d'entiers. *)
(* - dimK -> dimension du tableau sous la forme (nb_ligne,nb_colonne). *)
type tabKarnaugh = {tab: int list list; dimK: int*int};;

(*Construit un tableau de Karnaugh en fonction d'un formule.*)
let makeTab = function formule ->
  let nbvar = nb_variable formule in
  let listbin = List.map bin_of_mot (listMot formule) in
  let nbligne = pow 2 (nbvar/2) in
  let nbcolonne = pow 2 (nbvar-(nbvar/2)) in
  let rec makeLigne = fun l c resligne ->
    if c=nbcolonne
    then resligne
    else if List.mem ((remplissage (gray_of_dec l) (nbligne-1))^(remplissage (gray_of_dec c) (nbcolonne-1))) listbin
    then makeLigne l (c+1) (resligne@[1])
    else makeLigne l (c+1) (resligne@[0])
  in let rec aux = fun ligne restab ->
    if ligne=nbligne
    then {tab = restab ; dimK = (nbligne,nbcolonne)}
    else aux (ligne+1) (restab@[makeLigne ligne 0 []])
     in aux 0 [];;

(*Retourne la liste des coordonnees des "1" d'une matrice.*)
let listCoor = function tabK ->
  let rec makeCoor = fun l c list res -> match list with
      [] -> res
    | h::t -> if h=1
      then makeCoor l (c+1) t (res@[(l,c)])
      else makeCoor l (c+1) t res
  in let rec aux = fun ligne tab res -> match tab with
      [] -> res
    | h::t -> aux (ligne+1) t (res@(makeCoor ligne 0 h []))
     in aux 0 (tabK.tab) [];;

(*Retourne le tableau de Karnaugh sous forme de chaine de caracteres.*)
let afficher = function tabK ->
  let nb_ligne = fst tabK.dimK in
  let nb_colonne = snd tabK.dimK in
  let listCoor = listCoor tabK in
  let makeSegment = function colonne ->
    let rec aux = fun c res ->
      if c=0
      then res
      else aux (c-1) (res^"---+")
    in aux colonne "+" in
  let makeLine = function ligne ->
    let rec aux = fun c res ->
      if c >= nb_colonne
      then res
      else if List.mem (ligne,c) listCoor
      then aux (c+1) (res^" 1 |")
      else aux (c+1) (res^"   |")
    in aux 0 "|" in
  let rec makeTab = fun ligne res ->
    if ligne >= nb_ligne
    then res
    else makeTab (ligne+1) (res^"\t"^makeLine ligne^"\n"^"\t"^makeSegment nb_colonne^"\n")
  in makeTab 0 ("\t"^makeSegment nb_colonne^"\n");;


(*--------------------------------------------------*)
(* Fonctions de groupement des elements du tableau *)

(* Type element : *)
(* - coins -> ses 4 coins. *)
(* - listeElem -> contient toute les coordonnees. *)
(* - dim -> indique la dimension de l'element. *)

type coins = {
  hg : int * int;
  hd : int * int;
  bg : int * int;
  bd : int * int
};;

type listCoor = (int*int) list;;

type dim = int * int;;

type element = {coins:coins; listCoor:listCoor; dim:dim};;

type pr = Haut | Gauche | Bas | Droite | None;;

(*Accesseur pour obtenir la liste de coordonnees d'un element.*)
let getListCoor = function elem ->
  elem.listCoor;;

(*Indique si elem1 est plus petit, plus grand ou egal a elem2.*)
let compareElem = fun elem1 elem2 ->
  let taille1 = (fst elem1.dim * snd elem1.dim) in
  let taille2 = (fst elem2.dim * snd elem2.dim) in
  if taille1 > taille2
  then 1
  else if taille1 = taille2
  then 0
  else (-1);;

(*Cree une liste d'elements de dimension 1x1 a partir d'un tableau de Karnaugh.*)
let listElem = function tabK ->
  let rec aux = fun l res -> match l with
      [] -> res
    | h::t -> aux t (res@[{coins = {hg=h;hd=h;bg=h;bd=h}; listCoor=[h]; dim=(1,1)}])
  in aux (listCoor tabK) [];;

(*Indique si deux elements sont alignes.*)
let aligne = fun elem1 elem2 ->
  if ( ( ((fst elem1.coins.hg)=(fst elem2.coins.hg))&&((fst elem1.coins.bg)=(fst elem2.coins.bg)) )
       || ( ((snd elem1.coins.hg)=(snd elem2.coins.hg))&&((snd elem1.coins.hd)=(snd elem2.coins.hd)) ) )
  then true
  else false;;

(*Retourne la coordonnee voisine d'une coordonnee.*)
let voisin = fun p coor tabK ->
  match p with
      Haut ->
	if fst coor = 0
	then ((fst tabK.dimK - 1), snd coor)
	else ((fst coor - 1), snd coor)
    | Gauche ->
      if snd coor = 0
      then (fst coor, (snd tabK.dimK - 1))
      else (fst coor, (snd coor - 1))
    | Bas ->
      if fst coor = (fst tabK.dimK - 1)
      then (0, snd coor)
      else ((fst coor + 1), snd coor)
    | Droite ->
      if snd coor = (snd tabK.dimK - 1)
      then (fst coor, 0)
      else (fst coor, (snd coor + 1))
    | _ -> failwith "1er parametre incorrecte.";;

(*Calcule l'adjacence de deux elements dans un tableau tabK.*)
let adjacent = fun elem1 elem2 tabK->
  if ((elem1.dim <> elem2.dim) || (elem1.coins = elem2.coins))
  then None
  else if ((voisin Droite elem1.coins.bd tabK) = (elem2.coins.bg))
  then Droite
  else if ((voisin Bas elem1.coins.bd tabK) = (elem2.coins.hd))
  then Bas
  else if ((voisin Gauche elem1.coins.hg tabK) = (elem2.coins.hd))
  then Gauche
  else if ((voisin Haut elem1.coins.hg tabK) = (elem2.coins.bg))
  then Haut
  else None;;

(*Reunit deux elements adjacents et de meme dimension selon la position relative p de elem2 par rapport a elem1.*)
let reunir = fun elem1 elem2 tabK ->
  let p = (adjacent elem1 elem2 tabK) in
  if p = Haut
  then {coins = {hg=elem2.coins.hg; hd=elem2.coins.hd; bg=elem1.coins.bg; bd=elem1.coins.bd};
	listCoor = elem1.listCoor@elem2.listCoor;
	dim = ((fst elem1.dim + fst elem2.dim), snd elem1.dim)}
  else if p = Gauche
  then {coins = {hg=elem2.coins.hg; hd=elem1.coins.hd; bg=elem2.coins.bg; bd=elem1.coins.bd};
	listCoor = elem1.listCoor@elem2.listCoor;
	dim = (fst elem1.dim,(snd elem1.dim + snd elem2.dim))}
  else if p = Bas
  then{coins = {hg=elem1.coins.hg; hd=elem1.coins.hd; bg=elem2.coins.bg; bd=elem2.coins.bd};
       listCoor = elem1.listCoor@elem2.listCoor;
       dim = ((fst elem1.dim + fst elem2.dim), snd elem1.dim)}
  else if p = Droite
  then {coins = {hg=elem1.coins.hg; hd=elem2.coins.hd; bg=elem1.coins.bg; bd=elem2.coins.bd};
	listCoor = elem1.listCoor@elem2.listCoor;
	dim = (fst elem1.dim,(snd elem1.dim + snd elem2.dim))}
  else failwith "Regroupement impossible.";;

(*Construit la liste des reunion possibles entre un element et une liste d'elements.*)
let regrouper = fun elem listElem tabK ->
  let rec aux = fun l res -> match l with
      [] -> res
    | h::t -> if ((adjacent elem h tabK <> None))
      then aux t (res@[(reunir elem h tabK)])
      else aux t res
  in aux listElem [];;

(*Fabrique la liste de tous les elements possible d'un tableau de Karnaugh.*)

let listAllElem = function tabK ->
  let listDepart = listElem tabK in
  let rec aux = fun l ln res -> match l with
      [] ->
	begin
	  match ln with
	      [] -> res
	    | h::t -> aux ln [] res
	end
    | h::t -> if List.length l < 1
      then res
      else aux t (ln@(regrouper h t tabK)) (res@(regrouper h (l@t) tabK))
  in aux listDepart [] listDepart;;

(*Elimine les elements d'une liste incluent dans l'element elem.*)
let filtrer = fun elem listElem ->
  let rec aux = fun l res -> match l with
      [] -> res
    | h::t -> if (inclus h.listCoor elem.listCoor)
      then aux t res
      else aux t (res@[h])
  in aux listElem [];;

(*Elimine les elements dont toutes les coordonnees sont incluent dans l'ensemble des autres elements.*)
let filtrerFinal = function listElem ->
  let rec aux = fun l1 res -> match l1 with
      [] -> res
    | h::t -> if inclus (getListCoor h) (List.concat (List.map getListCoor (t@res)))
      then aux t res
      else aux t (res@[h])
  in aux listElem [];;

(*Resoud un tableau de Karnaugh, resultat sous forme de liste d'elements.*)
let resoudre = function tabK ->
  let liste = List.rev (List.sort compareElem (listAllElem tabK)) in
  let rec aux = fun l res ->
    if (List.length l) < 2
    then filtrerFinal (res@l)
    else aux (filtrer (List.hd l) (List.tl l)) (res@[List.hd l])
  in aux liste [];;


(*--------------------------------------------------*)
(* Fonctions de conversion d'un tableau de Karnaugh a une formule *)

exception Tautologie;;

(*Traduit une coordonnee en gray code par rapport a un tableau de Karnaugh.*)
let gray_of_coor = fun tabK coor ->
  (remplissage (gray_of_dec (fst coor)) (fst tabK.dimK - 1))^(remplissage (gray_of_dec (snd coor)) (snd tabK.dimK - 1));;

(*Conversion : liste gray code en mot simplifie*)
let mot_of_listGray = fun formule listGray ->
  let listVar = listVar formule in
  let rec simil = fun c l n -> match l with
      [] -> true
    | h::t -> if (String.get h n)=c
      then simil c t n
      else false in
  let rec aux = fun lg lv n res -> match lv with
      [] -> res
    | h::t -> if simil (String.get (List.hd lg) n) lg n
      then if (String.get (List.hd lg) n) = '1'
	then aux lg (List.tl lv) (n+1) (res^(string_of_char (List.hd lv)))
	else aux lg (List.tl lv) (n+1) (res^"!"^(string_of_char (List.hd lv)))
      else aux lg (List.tl lv) (n+1) res
  in aux listGray listVar 0 "";;

(*Renvoie une formule a partir d'une liste d'elements.*)
let solution = fun formule tabK listElem ->
  if (fst tabK.dimK * snd tabK.dimK) = List.length(ensemble (listMot formule))
  then raise Tautologie
  else
    let list_of_listCoor = List.map getListCoor listElem in
    let list_of_listGray = List.map (List.map (gray_of_coor tabK)) list_of_listCoor in
    let listMot = List.map (mot_of_listGray formule) list_of_listGray in
    let rec consFormule = fun l res ->
      match l with
	  [] -> res
	| h::t -> consFormule t (res^" + "^h)
    in if List.length  listMot = 1
      then List.hd listMot
      else consFormule (List.tl listMot) (List.hd listMot) ;;


(*--------------------------------------------------*)
(* Karnaugh *)

let rec karnaugh = function resultat ->
  print_string (resultat);
  print_string "Enter a boolean equation :\n -> ";
  let formule = read_line() in
  if formule = ""
  then print_string "Fin\n"
  else
    let tabK = makeTab formule in
    let solution =
      try solution formule tabK (resoudre tabK)
      with Tautologie -> "Vrai" in
    karnaugh
      ("##################################################\n\n"^
	  "\t"^afficheVar formule^"\n"^
	  afficher tabK^"\n"^
	  " Formule    : "^formule^"\n\n"^
	  " Simplifiée : "^solution^"\n\n"^
	  "##################################################\n\n");;

let main() = karnaugh "Methode de Karnaugh\n";;

main();;


(* TEST *)

let formuleTest = "!x!y!z!w + !x!yz!w + !xy!zw + !xyzw + !xyz!w + xy!zw + xyzw + xyz!w + x!y!z!w + x!yz!w";;
let tabTest = makeTab formuleTest;;
solution formuleTest tabTest (resoudre tabTest);;

let formuleD = "!x!y!z!w + !x!yz!w + !xy!zw + !xyzw + !xyz!w + xy!zw + xyzw + xyz!w + x!y!z!w + x!yz!w";;
let tabD = makeTab formuleD;;
tabD.tab;;
listCoor tabD;;
listElem tabD;;

let lcoor = [(0,1);(0,2);(1,2);(1,1)];;
let lgray = (List.map (gray_of_coor tabD) lcoor);;
mot_of_listGray formuleD lgray;;
listVar formuleD;;

let affichage = fun liste ->
  let rec aux = fun l res -> match l with
      [] -> res
    | h::t -> aux t (res@[(h.coins.hg, h.dim)])
  in aux liste [];;

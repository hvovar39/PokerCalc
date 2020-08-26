open Projet_comparaison ;;
open Projet_proba;;
open Char;;
open String;;
open Str;;


(* On lit une ligne du fichier et on le sépare en list de string grâce à READ_LINE. On prend ensuite c'est tableaux de string (3 tableau, 1 par ligne) et on les passes dans LANCEMENT, qui va vérifié si l'on a une donne2 puis transformé les strings en carte grâce à TO_VAL et TO_COUL et lance les calcules de probabilités*)
  
  
let fichier= open_in Sys.argv.(1);;

let to_val s=match s with
  |"A" -> A
  |"R" -> R
  |"D" -> D
  |"V" -> V
  |"10" -> Nombre 10
  |"9" -> Nombre 9
  |"8" -> Nombre 8
  |"7" -> Nombre 7
  |"6" -> Nombre 6
  |"5" -> Nombre 5
  |"4" -> Nombre 4
  |"3" -> Nombre 3
  |"2" -> Nombre 2
  |a -> raise (Failure a)
;;

let to_coul s=match s with
  |"co" -> Co
  |"ca" -> Ca
  |"t" -> Tr
  |"p" -> Pi
  |_ -> NilC

let rec to_carte ls = match ls with
  |[] -> []
  |s::q ->
    if ( (length s)=3) then
      if(s.[0]='1') then ( (to_carte q)@[(to_val ((Char.escaped s.[0])^(Char.escaped s.[1])), to_coul (Char.escaped s.[2]))] )
      else (to_carte q)@[(to_val (Char.escaped s.[0]), to_coul ((Char.escaped s.[1])^(Char.escaped s.[2])))]
             
    else if( (length s)=4) then (to_carte q)@[(Nombre 10, to_coul ((Char.escaped s.[2])^(Char.escaped s.[3])))]
    else
      (to_carte q)@[(to_val (Char.escaped s.[0]), to_coul (Char.escaped s.[1]))]
;;

  
let lance_proba_double d1 d2 t=match (probaDouble d1 d2 t) with
  |(v1, v2) -> output_string stdout ("Le joueur 1 à une probabilité de gagné de : "^(string_of_float v1)^"\nLe joueur 2  une probabilité de gagné de : "^(string_of_float v2)^"\n")
;;

let lance_proba_simple d1 t=
  output_string stdout ("Le joueur 1 à une probabilité de gagné de : "^( string_of_float (probaSimple d1 t)))
;;

let lancement d1 d2 t= match (d1, d2, t) with
  |(a, (["?"]), t2 ) -> (match (to_carte a) with
                        |d::f -> lance_proba_simple (d, (List.hd f)) (to_carte t2)
                        |_ -> raise (Failure "erreur de lancement1") )
  |(a, q , _) ->  (match (to_carte a, to_carte q) with
                   | (d::f,p::s)-> lance_proba_double ( d, (List.hd f) ) ( p, (List.hd s) ) (to_carte t)
                   | _ -> raise (Failure "erreur de lancement2"))
;;
  
  
let lecture_ligne f = split (regexp "[ \t]") (input_line f)
;;
  



let test = lancement  (lecture_ligne fichier) (lecture_ligne fichier) (lecture_ligne fichier);;

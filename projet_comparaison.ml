open List ;;
 
(* DEFIITION DES TYPES DE BASES *)

type valeur = NilV
            |Nombre of int
            |A
            |R
            |D
            |V
;;

type couleur = NilC
             |Tr
             |Pi
             |Co
             |Ca
;;

type carte = valeur * couleur
;;
  
type donne = carte * carte
;;

type table = Pre_flop
           |Flop of carte list
           |Turn of carte list
           |River of carte list
;;
  
type combinaison = Nil
                 |Quinte_flush of valeur
                 |Carre of valeur
                 |Full of valeur * valeur
                 |Couleur of valeur * valeur * valeur * valeur * valeur
                 |Suite of valeur
                 |Brelan of valeur
                 |Double_paire of valeur * valeur * valeur
                 |Paire of valeur * valeur * valeur * valeur
                 |Carte_haute of valeur * valeur * valeur * valeur * valeur
;;
  


  (*FONCTION DE COMPARAISON*)

  (* COMPAR_COMB   Comparaison de deux combinaison c1 et c2. Renvoie 1 si c1>c2, -1 si c2>c1, 0 si c1=c2*)

(*comparaison de valeur v1 v2, renvoie 1 si v1>v2, 0 si v1=v2, -1 si v2>v1*)
let compar_valeur v1 v2= match (v1,v2) with
  |(A, _) -> if (v2==A) then 0
             else 1
  |(_, A) -> -1
  |(R, _) -> if (v2==R) then 0
            else 1
  |(_, R) -> -1
  |(D, _) -> if(v2==D) then 0
             else 1
  |(_, D) -> -1
  |(V, _) -> if(v2==V) then 0
             else 1
  |(_, V) -> -1
  |(n1, n2) -> if(n1>n2) then 1
               else if(n2>n1) then -1
               else 0
;;

(* Fonction permettant de comparé plusieurs valeurs les unes à la suite des autres *)
let rec compar_liste l1 l2= match (l1, l2) with
  |([], _::_) -> raise (Failure "Pas assez de valeur dans la premiere liste pour effectuez la comparaison")
  |(_::_, []) -> raise (Failure "Pas assez de valeur dans la seconde liste pour effectuez la comparaison")
  |([], []) -> 0
  |(t1::q1, t2::q2) -> let res=compar_valeur t1 t2 in
                       if(res==0) then compar_liste q1 q2
                       else res
;;
  
  
   
let compar_comb c1 c2 = match (c1, c2) with
  |(Quinte_flush q1, Quinte_flush q2) -> compar_valeur q1 q2
  |(Quinte_flush q1, _) -> 1
  |(_, Quinte_flush q) -> -1
                           
  |(Carre ca1, Carre ca2) -> compar_valeur ca1 ca2
  |(Carre c, _) -> 1
  |(_, Carre c) -> -1
                    
  |(Full (v11, v12) , Full (v21, v22)) -> compar_liste [v11; v12] [v21; v22]
  |(Full (v11, v12), _) -> 1
  |(_, Full (v21, v22)) -> -1
                   
  |(Couleur (v11, v12, v13, v14, v15), Couleur (v21, v22, v23, v24, v25)) -> compar_liste [v11; v12; v13; v14; v15] [v21; v22; v23; v24; v25] 
  |(Couleur (v11, v12, v13, v14, v15), _) -> 1
  |(_, Couleur (v11, v12, v13, v14, v15)) -> -1
                      
  |(Suite s1, Suite s2) -> compar_valeur s1 s2
  |(Suite s1, _) -> 1
  |(_, Suite s2) -> -1
                    
  |(Brelan b1, Brelan b2) -> compar_valeur b1 b2
  |(Brelan b1, _) -> 1
  |(_, Brelan b2) -> -1
                     
  |(Double_paire (v11, v12, v13), Double_paire (v21, v22, v23)) -> compar_liste [v11; v12; v13] [v21; v22; v23]
  |(Double_paire (v11, v12, v13), _) -> 1
  |(_, Double_paire (v21, v22, v23)) -> -1
                           
  |(Paire (v11, v12, v13, v14), Paire (v21, v22, v23, v24)) -> compar_liste [v11; v12; v13; v14] [v21; v22; v23; v24]
  |(Paire (v11, v12, v13, v14), _) -> 1
  |(_, Paire (v21, v22, v23, v24)) -> -1
                    
  |(Carte_haute (v11, v12, v13, v14, v15), Carte_haute (v21, v22, v23, v24, v25)) -> compar_liste [v11; v12; v13; v14; v15] [v21; v22; v23; v24; v25]
  |(_, _) -> 0
;;
  



  (* CARTE_TO_COMB permet de passé d'un ensemble de 5 carte à un combo (la plus "puissant" que l'on puisse faire avec c'est 5 cartes.
Pour cela on passe par TRI_CARTE, qui va trié les 5 cartes en fonctions de leur valeurs*)

let rec tri_carte l = match l with
  |[] -> []
  |[a] -> l
  |t::q -> insertion_carte t (tri_carte q)

and insertion_carte c l = match l with
  |[] -> [c]
  |t::q -> if((compar_valeur (fst c) (fst t))=1) then c::l
           else t::(insertion_carte c q)
;;

(*Test des différentes combinaison pour carte_to_comb*)
let test_couleur l = match l with
  |[(_, co1); (_, co2); (_, co3); (_, co4); (_, co5)] -> (co1==co2 && co1==co3 && co1==co4 && co1==co5)
  |_ -> false
;;
    
let test_quinte l= let flush = test_couleur l in
                   match l with
                   |[(A, _); (R, _); (D, _); (V, _); (Nombre 10, _)] -> if flush then 0 else 1
                   |[(R, _); (D, _); (V, _); (Nombre 10, _); (Nombre 9, _)] -> if flush then 0 else 1
                   |[(D, _); (V, _); (Nombre 10, _); (Nombre 9, _); (Nombre 8, _)] -> if flush then 0 else 1
                   |[(V, _); (Nombre 10, _); (Nombre 9, _); (Nombre 8, _); (Nombre 7, _)] -> if flush then 0 else 1
                   |[(Nombre 5, _); (Nombre 4, _); (Nombre 3, _); (Nombre 2, _); (A, _)] -> if flush then 0 else 1
                   |[(Nombre n1, _); (Nombre n2, _); (Nombre n3, _); (Nombre n4, _); (Nombre n5, _)] -> if(n1==(n2+1) && n2==(n3+1) && n3==(n4+1) && n4==(n5+1)) then (if flush then 0 else 1) else -1
                   |_ -> -1
;;
  
let rec test_carre l=match l with
  |[c1; c2; c3; c4] -> if( (compar_valeur (fst c1) (fst c2))==0 && (compar_valeur (fst c1) (fst c3))==0 && (compar_valeur (fst c1) (fst c4))==0) then Carre (fst c1)
                       else Nil                              
  |[c1; c2; c3; c4; c5] -> if( (compar_valeur (fst c1) (fst c2))!=0) then test_carre [c2; c3; c4; c5]
                           else test_carre [c1; c2; c3; c4]
  |_ -> Nil
;;

let test_full l=match l with
  |[(v1, _); (v2, _); (v3, _); (v4, _); (v5, _)] -> if( (compar_valeur v1 v2)==(compar_valeur v2 v3) && (compar_valeur v1 v2)==0 && (compar_valeur v4 v5)==0 ) then Full (v1, v4)
                                                    else if ( (compar_valeur v3 v4)==(compar_valeur v4 v5) && (compar_valeur v4 v5)==0 && (compar_valeur v1 v2)==0 ) then Full (v4, v1)
                                                    else Nil
  |_ -> Nil
;;

let rec test_brelan l=match l with
  |[(v1, _); (v2, _); (v3, _)] -> if( (compar_valeur v1 v2)==0 && (compar_valeur v2 v3)==0 ) then Brelan v1
                                  else Nil
  |(v1, _)::((v2, cl1)::((v3, cl2)::q)) -> if( (compar_valeur v1 v2)==0 && (compar_valeur v2 v3)==0 ) then Brelan v1
                                       else test_brelan ((v2, cl1)::((v3, cl2)::q))
  |_ -> Nil
;;

let rec test_paire l=match l with
  |[(v1, _); (v2, _); (v3, _); (v4, _); (v5, _)] -> if( (compar_valeur v1 v2)==0 ) then Paire (v1, v3, v4, v5)
                                                    else if( (compar_valeur v2 v3)==0 ) then Paire (v2, v1, v4, v5)
                                                    else if( (compar_valeur v3 v4)==0 ) then Paire (v3, v1, v2, v5)
                                                    else if( (compar_valeur v4 v5)==0 ) then Paire (v4, v1, v2, v3)
                                                    else Nil
  |_ -> Nil
;;
  
  
let carte_to_comb c1 c2 c3 c4 c5=
  let liste_carte=tri_carte [c1; c2; c3; c4; c5] in
  let quinte=test_quinte liste_carte in
  match liste_carte with
  |[l1; l2; l3; l4; l5] ->
    if (quinte==1) then Suite (fst l1)
    else if(quinte==0) then Quinte_flush (fst l1)
    else
      let comb1= test_carre liste_carte in
      if(comb1 != Nil) then comb1
      else
        let comb2= test_full liste_carte in
        if(comb2 != Nil) then comb2
        else
          if(test_couleur liste_carte) then Couleur ((fst l1), (fst l2), (fst l3), (fst l4), (fst l5))
          else
            let comb3=test_brelan liste_carte in
            if(comb3 != Nil) then comb3
            else
              let comb4=test_paire liste_carte in
              if(comb4 != Nil) then ( match comb4 with
                                      |Paire (v1, v2, v3, v4) -> if ( (compar_valeur v2 v3)==0 ) then Double_paire (v1, v2, v4)
                                                                 else if( (compar_valeur v3 v4)==0) then Double_paire (v1, v3, v2)
                                                                 else comb4
                                      |_ -> Nil )
              else
                Carte_haute ((fst l1), (fst l2), (fst l3), (fst l4), (fst l5))
  |_ -> raise (Failure "Il n'y à pas le bon nombre de carte pour faire un combo\n")
;;
          
      
  
(*  COMPUTE_COMB  renvoie la combinaison la plus forte que l'on peut faire avec une donne et une table de 5 cartes *)

let rec bin_to_comb liste_bin liste_carte res = match liste_bin with
  |[] -> (match res with
          |[c1; c2; c3; c4; c5] -> carte_to_comb c1 c2 c3 c4 c5
          |_ -> raise (Failure "porblème de compilation de carte")
         )
  |t::q -> if(t==1) then bin_to_comb q (tl liste_carte) ((hd liste_carte)::res)
           else bin_to_comb q (tl liste_carte) res
;;
  
let rec comb_max compteur liste_binaire liste_carte c_max=
  if (compteur==0) then c_max
  else if (compteur==20) then
    let comb_temp=bin_to_comb liste_binaire liste_carte [] in
    if( (compar_comb comb_temp c_max)>=0) then comb_max (compteur-1) liste_binaire liste_carte comb_temp
    else comb_max (compteur-1) liste_binaire liste_carte c_max
  else let rec b1 t q = if ((length q)>1) then
                          if ((hd q)>=(hd (tl q))) then b1 (t@[hd q]) (tl q)
                          else (t,  tl (tl q))
                        else (t,  [])
       in
       let temp=b1 [] (rev liste_binaire) in
       let new_list_bin= rev ((rev (fst temp))@[1;0]@(snd temp)) in
       let comb_temp=bin_to_comb new_list_bin liste_carte [] in
       if( (compar_comb comb_temp c_max)>=0) then comb_max (compteur-1) new_list_bin liste_carte comb_temp
       else comb_max (compteur-1) new_list_bin liste_carte c_max
;;
  
let compar_hands done1 done2 table = match (done1,done2,table) with
  |((c1,c2),(c3,c4),[t1;t2;t3;t4;t5])->compar_comb (comb_max 20 [1;1;1;1;1;0;0] [c1;c2;t1;t2;t3;t4;t5] Nil) (comb_max 20 [1;1;1;1;1;0;0] [c3;c4;t1;t2;t3;t4;t5] Nil)
  |_->raise(Failure"ils manque des cartes !");;

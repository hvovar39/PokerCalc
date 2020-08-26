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




















  










  let jeux =[(Nombre 2, Co);(Nombre 3, Co);(Nombre 4, Co);(Nombre 5, Co);(Nombre 6, Co);(Nombre 7, Co);(Nombre 8, Co);(Nombre 9, Co);(Nombre 10, Co);(V, Co);(D, Co);(R, Co);(A, Co);(Nombre 2, Ca);(Nombre 3, Ca);(Nombre 4, Ca);(Nombre 5, Ca);(Nombre 6, Ca);(Nombre 7, Ca);(Nombre 8, Ca);(Nombre 9, Ca);(Nombre 10, Ca);(V, Ca);(D, Ca);(R, Ca);(A, Ca);(Nombre 2, Tr);(Nombre 3, Tr);(Nombre 4, Tr);(Nombre 5, Tr);(Nombre 6, Tr);(Nombre 7, Tr);(Nombre 8, Tr);(Nombre 9, Tr);(Nombre 10, Tr);(V, Tr);(D, Tr);(R, Tr);(A, Tr);(Nombre 2, Pi);(Nombre 3, Pi);(Nombre 4, Pi);(Nombre 5, Pi);(Nombre 6, Pi);(Nombre 7, Pi);(Nombre 8, Pi);(Nombre 9, Pi);(Nombre 10, Pi);(V, Pi);(D, Pi);(R, Pi);(A, Pi)];;

  (*Fonction permettant de verifier si une carte fait partie du jeux*)


    let rec memAndChange a l =
      if (mem a l) then match l with
                        |[]-> []
                        |t :: q-> if t=a then (NilV,NilC)::q  else t::memAndChange a q
      else raise(Failure "Carte introuvable ou deja sortie \n")
;; 

let rec test listCarte jeux= match listCarte with
  |[]->jeux
  |t::q->test q (memAndChange t jeux)
;;


let rec deuxCombinaison t = match t with
  | [] -> []
  | d :: f ->let tab acc elt = [d; elt] :: acc in fold_left tab (deuxCombinaison f) (rev f);;

(* trois sera utiliser quand la table contient 3 carte, le but est quelle renvoi les proba pour (proba j1,proba j2)*)
(*2 parmis 45 (les 52 carte moins les donnes et la table) donne 990.0*)
let trois d1 d2 t ct1 ct2 = match (d1,d2,t)with
  |((c1,c2),(c3,c4),[t1;t2;t3])->let couple=deuxCombinaison (test [c1;c2;c3;c4;t1;t2;t3] jeux) in
                                    let rec bis ct1 ct2 couple= ( match couple with
                                                                 |[]->(ct1/.990.0,ct2/.990.0)
                                                                 |d::f->( match d with
                                                                         |[]->bis ct1 ct2 f
                                                                         |[ft;sd]-> if (ft=(NilV,NilC) || sd=(NilV,NilC)) then bis ct1 ct2 f
                                                                                    else
                                                                                      let tComplet= [t1;t2;t3;ft;sd] in
                                                                                      if ((compar_hands d1 d2 tComplet)=1) then let ct3=ct1+.1.0 in bis ct3 ct2 f
                                                                                      else if ((compar_hands d1 d2 tComplet)= (-1)) then let ct3= ct2+.1.0 in bis ct1 ct3 f
                                                                                      else bis ct1 ct2 f
                                                                         |_ -> raise (Failure "Probleme de couple") ) )
                                    in bis ct1 ct2 couple
  |_->raise(Failure"Carte incorrecte")
;;

let troisBis d1 t ct1=match(d1,t)with
  |((c1,c2),[t1;t2;t3])->let doneJ2=deuxCombinaison (test [c1;c2;t1;t2;t3] jeux) in
                         let rec bis ct1 doneJ2=(match doneJ2 with
                                                 |[]->ct1
                                                 |d::f->(match d with
                                                         |[ft;sd]-> if (ft=(NilV,NilC) || sd=(NilV,NilC)) then bis ct1 f
                                                                    else let couple=deuxCombinaison (test [c1;c2;ft;sd;t1;t2;t3] jeux) in
                                                                         let rec biss ct1 couple= ( match couple with
                                                                                                           |[]->ct1
                                                                                                           |tt::q->( match tt with
                                                                                                                     |[f2;s]-> if (f2=(NilV,NilC) || s=(NilV,NilC)) then biss ct1 q
                                                                                                                               else
                                                                                                                                 let d2= (ft,sd) and tcomp=t@[f2;s] in
                                                                                                                                 if ((compar_hands d1 d2 tcomp)=1) then let ct2=ct1+.1.0 in biss ct2 q
                                                                                                                                 else biss ct1 q
                                                                                                                     |_ -> raise (Failure "Probleme de couple") ) )
                                                                         in bis (biss ct1 couple) f
                                                         |_ -> raise (Failure "Probleme de couple") ))
                         in (bis ct1 doneJ2)/.(1081.0*.990.0)
  |_->raise(Failure"Carte incorrecte")
;;

  
(* quatre sera utiliser quand la table contient 4 carte, le but est quelle renvoi les proba pour (proba j1,proba j2)*)
  
let quatre d1 d2 t ct1 ct2= match (d1,d2,t)with
  |((c1,c2),(c3,c4),[t1;t2;t3;t4])->let carteRest=test [c1;c2;c3;c4;t1;t2;t3;t4] jeux in
                                    let rec bis ct1 ct2 carteRest= (match carteRest with
                                      |[]->(ct1/.44.0,ct2/.44.0)
                                      |d::f->if (d=(NilV,NilC)) then bis ct1 ct2 f
                                             else let tComplet= [t1;t2;t3;t4;d] in
                                                  if ((compar_hands d1 d2 tComplet)=1) then let ct3=ct1+.1.0 in bis ct3 ct2 f
                                                  else if ((compar_hands d1 d2 tComplet)= (-1)) then let ct3= ct2+.1.0 in bis ct1 ct3 f
                                                  else bis ct1 ct2 f)
                                    in bis ct1 ct2 carteRest
  |_->raise(Failure "Carte incorrecte");;


 let quatreBis d1 t ct1=match(d1,t)with
  |((c1,c2),[t1;t2;t3;t4])->let doneJ2=deuxCombinaison (test [c1;c2;t1;t2;t3;t4] jeux) in
                         let rec bis ct1 doneJ2=(match doneJ2 with
                                                 |[]->ct1/.(1035.0*.44.0)
                                                 |d::f->(match d with
                                                         |[ft;sd]-> if (ft=(NilV,NilC) || sd=(NilV,NilC)) then bis ct1 f
                                                                    else let carteReste=test [c1;c2;ft;sd;t1;t2;t3;t4] jeux in
                                                                         let rec biss ct1 carteReste= ( match carteReste with
                                                                                                    |[]->ct1
                                                                                                    |tt::q->if (tt=(NilV,NilC)) then biss ct1 q
                                                                                                            else
                                                                                                              let d2= (ft,sd) and tcomp=t@[tt] in
                                                                                                               if ((compar_hands d1 d2 tcomp)=1) then let ct2=ct1+.1.0 in biss ct2 q
                                                                                                              else biss ct1 q)
                                                                         in bis (biss ct1 carteReste) f 
                                                         |_ -> raise (Failure "Probleme de couple")))
                         in bis ct1 doneJ2
  |_->raise(Failure"Carte incorrecte")
 ;;
                                                   
(* cinq  sera utiliser quand la table contient 5 carte, le but est quelle renvoi les proba pour (proba j1,proba j2)*)
  
 let cinq d1 d2 t =  match (d1,d2,t)with
  |((c1,c2),(c3,c4),[t1;t2;t3;t4;t5])->if ((compar_hands d1 d2 t)=1) then (1.0,0.0)
                                                  else if ((compar_hands d1 d2 t)= (-1)) then (0.0,1.0)
                                       else (0.5,0.5)
  |_->raise(Failure"Carte incorrecte");;

let cinqBis d1 t ct1=match(d1,t)with
  |((c1,c2),[t1;t2;t3;t4;t5])->let doneJ2=deuxCombinaison (test [c1;c2;t1;t2;t3;t4;t5] jeux) in
                         let rec bis ct1 doneJ2=(match doneJ2 with
                                                     |[]->(ct1/.990.0)
                                                     |d::f->(match d with
                                                             |[ft;sd]-> if (ft=(NilV,NilC) || sd=(NilV,NilC)) then bis ct1 f
                                                                        else if (let d2=(ft,sd) in (compar_hands d1 d2 t)=1) then let ct2=ct1+.1.0 in bis ct2 f
                                                                        else bis ct1 f
                                                             |_ -> raise (Failure "Probleme de couple") ))
                         in bis ct1 doneJ2
  |_->raise(Failure"Carte incorrecte")
;;




(*probaDouble renvoi un couple (proba de victoire de j1, proba de victoire de j2) que l'on ai de 3 à 5 carte sur la table*)
  
let probaDouble d1 d2 t= match length t with
  |3->(trois d1 d2 t 0.0 0.0)
  |4->(quatre d1 d2 t 0.0 0.0)
  |5->(cinq d1 d2 t)
  |_->raise(Failure"la table ne contient pas un nombre reglementaire de carte!")
;;

let probaSimple d1 t=match length t with
  |3->(troisBis d1 t 0.0)
  |4->(quatreBis d1 t 0.0)
  |5->(cinqBis d1 t 0.0)
  |_->raise(Failure"la table ne contient pas un nombre reglementaire de carte!")
;;















(*OPTION INTERACTION AVEC LES JOUEURS*)

(*Pioche une carte dans un jeux*)
let rec piocheUneCarte j i= match (j,i) with
  |(t::q,i)->if (t=(NilV,NilC)) then piocheUneCarte q (i-1) else if (i=0) then t else piocheUneCarte q (i-1)
  |([],_)->raise(Failure"le nombre i n'est pas conforme")
;;
  
(*pioche n carte dans un jeux *)
let rec pioche n j r=if n= 0 then r else let i=Random.int(length j) in let carte =piocheUneCarte j i in let newJ =test [carte] j in let res=carte::r in pioche (n-1) newJ res;;

(*Les fonctions suivante permettent de transformer une list de carte en string et de l'afficher*)
  
let string_of_couleur c=match c with
  |NilC->raise (Failure "couleur invalide")
  |Tr->"Trefle"
  |Pi->"Pique"
  |Co->"Coeur"
  |Ca->"Carreaux"
;;

let string_of_valeur v=match v with
  |NilV->raise (Failure "couleur invalide")
  |A->"As"
  |R->"Roi"
  |D->"Dame"
  |V->"Valet"
  |Nombre i->string_of_int i
;;

         
let string_of_carte carte=match carte with
  |(v,c)->string_of_valeur v^" de "^string_of_couleur c
;;
  
let rec print_list l= match l with
  |[]->()
  |t::q->output_string stdout ( string_of_carte  t ) ;
         output_char stdout '\n' ;
         print_list q
;;



let rec demanderUneMise m1 m2 =let s=string_of_int m2 in print_endline (s^" est la mise minimum, vous pouvez decidé de suivre (en ecrivant suivre), de relancer (en écrivant relancer, en appuyant sur entrer, en donner votre relance, en appuyant sur entrer puis ;;), ou de vous coucher (en écrivant coucher)");
                                              let res=read_line() in match res with
                                                                     |"suivre;;"->m1
                                                                     |"relancer;;"->(int_of_string (read_line()))+m1
                                                                     |"coucher;;"->(-1)
                                                                     |_->print_endline ("je ne comprend pas ce que vous vouler");
                                                                         demanderUneMise m1 m2
;;
  
(* renvoi la mise si les deux joueur se sont aligné, -1 si le joueur 2 s'est coucher et que le joueur 1 à gagner (respectivement -2 si c'est le joueur 1 qui s'est couché)*)                                                                                                                                                                                            
let rec tourDeMise m1 m2 m3 t =if (t=0 || t=1) then let m= demanderUneMise m1 m3 in tourDeMise m m2 m (t+1)
                          else if (m1<m2) then let m=demanderUneMise m2 m3 in (if (m=(-1)) then (-2) else tourDeMise m m2 m (t+1))
                          else if (m1=m2) then m1
                          else let m=demanderUneMise m1 m3 in (if (m=(-1)) then (-1) else tourDeMise m1 m m (t+1));;

let rec demanderUneProba d1 t s = print_endline (s^" voulez vous voir votre probabilité de gagner ?");
                                  let res=read_line () in (match res with
                                                           |"oui;;"->print_endline ("l'adversaire dois se retourner, dite ok lorsque vous êtes prêt a lire la proba");
                                                                   if (read_line()="ok;;") then probaSimple d1 t
                                                                   else demanderUneProba d1 t s
                                                           |"non;;"->(-1.0)
                                                           |_->demanderUneProba d1 t s)
;;

let rec tour d1 d2 m m2 m3 t s=if m= (-1) then print_endline ("Le joueur 1 gagne !, vous pouvez quitter la fonction") 
                               else if m =(-2) then print_endline("le joueur 2 gagne, vous pouvez quitter le jeux")
                   else print_endline ("La mise est de "^string_of_int (m+m2+m3)^" les carte du "^s^" sont :");
                         print_list t;
                         let p1=demanderUneProba d1 t "Joueur 1" in (if (p1!=(-1.0)) then print_endline (string_of_float p1));
                                                                    let p2=demanderUneProba d2 t "Joueur 2" in (if (p2!=(-1.0)) then print_endline (string_of_float p2));
;;
  
(*fonction qui deroule un jeux*)

  
let jeuxDeroulement j=let d1bis=pioche 2 j [] in
           let newJ=test d1bis j in
           let d2bis=pioche 2 newJ [] in
                       let newJ2=test d2bis newJ in
                       let flop=pioche 3 newJ2 [] in
                       let newJ3=test flop newJ2 in
                       let tpm=pioche 1 newJ3 [] in
                       let newJ4=test tpm newJ3 in
                       let turn=flop@tpm in 
                       let tmp2=pioche 1 newJ4 [] in
                       let river=turn@tmp2 in
                       let d1=match d1bis with
                         |t::q->(t,hd q)
                         |_->raise(Failure"done non conforme")in
                       let d2=match d2bis with
                         |t::q->(t,hd q)
                         |_->raise(Failure"done non conforme")in

                       let rec voirCarte1 j1= print_endline ("Bonjour ! bienvenu dans cette nouvelle partie de Texas hold'hem ! \n Le"^j1^" peux regarder l'ecran (le deuxieme joueur dois se detourner le temps que le premier joueur voit ses carte etes vous pret ?repondre oui puis entrer puis ;; si c'est le cas\n");
                                              let s= read_line () in match s with
                                                                     |"oui;;"-> print_list d1bis
                                                                     |_->voirCarte1 j1
                       in voirCarte1 "joueur 1";
                          print_endline("\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n");
                          let rec voirCarte2 j2=print_endline ("Le"^ j2^" peux regarder l'ecran (le premier joueur dois se detourner le temps que le"^j2^" voit ses carte etes vous pret ? repondre oui puis entrer ;; puis  entrer quand c'est le cas\n");
                                                let s= read_line () in match s with
                                                                       |"oui;;"-> print_list d2bis
                                                                       |_->voirCarte2 j2
                          in voirCarte2 "joueur 2";
                             print_endline("\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n");
                             let mise= tourDeMise 0 0 0 0 in
                             tour d1 d2 mise 0 0  flop "flop";
                             print_endline("on rapelle que la mise actuel est de "^string_of_int mise);
                             let mise2=tourDeMise 0 0 mise 0 in
                             tour d1 d2 mise2 mise 0  turn "turn";
                                              print_endline("on rapelle que la mise actuel est de "^string_of_int (mise+mise2));
                                              let mise3=tourDeMise 0 0 mise2 0 in
                                              tour d1 d2 mise3 mise2 mise river "river";
                                              print_endline("on rapelle que la mise actuel est de "^string_of_int (mise+mise2+mise3));
                                              let mise4=tourDeMise 0 0 mise3 0 in
                                                           if mise= (-1) then print_endline ("Le joueur 1 gagne !")
                                                           else if mise =(-2) then print_endline ("le joueur 2 gagne")
                                                           else print_endline ("La mise est de "^string_of_int (mise+mise2+mise3+mise4));
                                                           let final=compar_hands d1 d2 river in
                                                           if (final=1) then print_endline ("Le joueur 1 à gagner la mise !")
                                                           else if (final=(-1)) then print_endline ("Le joueur 2 à gagner la mise !")
                                                           else print_endline ("Egalité !");
;;
  
  jeuxDeroulement jeux;;

   
    
 

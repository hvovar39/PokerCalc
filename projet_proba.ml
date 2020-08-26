open List;;
open Projet_comparaison;;

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
  |_->raise(Failure ("la table ne contient pas un nombre reglementaire de carte!"^(string_of_int(length t))))
;;
        

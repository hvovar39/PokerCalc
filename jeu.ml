open Projet_comparaison;;
open Projet_proba;;
open List;;
  
(*OPTION INTERACTION AVEC LES JOUEURS*)

(*Pioche une carte dans un jeux*)
let rec piocheUneCarte j i= match (j,i) with
  |(t::q, c)->if (c=0) then t
              else piocheUneCarte q (c-1)
  |([],_)->raise(Failure"le nombre i n'est pas conforme")
;;
  
(*pioche n carte dans un jeux *)
let rec pioche n j r=if n= 0 then r else let i=Random.int(length j) in let carte =piocheUneCarte j i in if carte=(NilV,NilC) then pioche n j r else let newJ =test [carte] j in let res=carte::r in pioche (n-1) newJ res;;

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



let rec demanderUneMise m1 m2 =let s=string_of_int m2 in print_endline (s^" est la mise minimum, vous pouvez decidé de suivre (en ecrivant suivre;;), de relancer (en écrivant relancer;;, en appuyant sur entrer, en donner votre relance, en appuyant sur entrer puis ;;), ou de vous coucher (en écrivant coucher;;)");
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
                                                           |"oui;;"->print_endline ("l'adversaire dois se retourner, dite ok;; lorsque vous êtes prêt a lire la proba");
                                                                   if (read_line()="ok;;") then probaSimple d1 t
                                                                   else demanderUneProba d1 t s
                                                           |"non;;"->(-1.0)
                                                           |_->demanderUneProba d1 t s)
;;

let rec tour d1 d2 m m2 m3 t s=if m= (-1) then print_endline ("Le joueur 1 gagne !, vous pouvez quitter la fonction") 
                               else if m =(-2) then print_endline("le joueur 2 gagne, vous pouvez quitter le jeux")
                   else print_endline ("La mise est de "^string_of_int (m+m2+m3)^" les carte du "^s^" sont :");
                         print_list t;
                         let p1=demanderUneProba d1 t "Joueur 1" in (if (p1!=(-1.0)) then print_endline (string_of_float p1 ^ " \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n"));
                                                                    let p2=demanderUneProba d2 t "Joueur 2" in (if (p2!=(-1.0)) then print_endline (string_of_float p2^ " \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n"));
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

                       let rec voirCarte1 j1= print_endline ("Bonjour ! bienvenu dans cette nouvelle partie de Texas hold'hem ! \n Le"^j1^" peux regarder l'ecran (le deuxieme joueur dois se detourner le temps que le premier joueur voit ses carte etes vous pret ?repondre oui;; si c'est le cas\n");
                                              let s= read_line () in match s with
                                                                     |"oui;;"-> print_list d1bis
                                                                     |_->voirCarte1 j1
                       in voirCarte1 "joueur 1";
                          print_endline("\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n  \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n");
                          let rec voirCarte2 j2=print_endline ("Le"^ j2^" peux regarder l'ecran (le premier joueur dois se detourner le temps que le"^j2^" voit ses carte etes vous pret ? repondre oui;; puis  entrer quand c'est le cas\n");
                                                let s= read_line () in match s with
                                                                       |"oui;;"-> print_list d2bis
                                                                       |_->voirCarte2 j2
                          in voirCarte2 "joueur 2";
                             print_endline("\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n  \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n\n \n \n \n \n \n \n \n \n \n \n \n");
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

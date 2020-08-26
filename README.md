Projet de programmation fonctionelle
Marianne LEMOINNE
Hugo VOVARD

Pour compiler le programme : dans un terminal, se rendre dans le répertoir contenant les fichiers, et entrer la commande "make"

Pour éxecuter les exemples : entrer "./compute" suivit du nom du fichier test

Pour lancer le jeu à deux : entrer "./jeu"

INFO IMPORTANTE: le programme contient quelque bug connue mais que nous n'avons pas eu le temps de corriger. la commade compute ne donne pas les bon résultats, pourtant nos fonctions probaSimple et probaDouble donne les bons résultats. Vous pouvez tester nos fonctions de probabilité en copiant collant les lignes suivantes dans le top level d'ocaml apres compiltion.

let test1=probaDouble ((A,Co),(Nombre 4,Pi)) ((R,Co),(A,Tr)) [(Nombre 2,Tr);(Nombre 3, Ca);(D,Tr);(V,Co);(Nombre 6,Pi)]
let test22=probasimple ((R,Co),(Nombre 2,Pi)) [(Nombre 7,Co);(Nombre 8, Tr);(R,Tr);(V,Ca);(Nombre 6,Pi)];;


jeu comporte lui aussi quelque bug, mais reste jouable. Pour quitter le jeu, veuillez utilisé ctrl+c. Dep plus pendant la partie pour ppourrez choisir si le calcule de vs chance de gagner se font ou non, sachez qu'il vaut demander les probabilités à partir du turn car e calcul de probabilité en phase flop peut metttre de trois à cinq minutes du faite de la quantité de calcule éxecuté lors de l'appelle à proba simple avec une table de 3 cartes.

Si vous voulez recompiler le programme, utiliser la commande make clean pour upprimer les fichier créé lors de la compilation précédente, puis reancer un make.


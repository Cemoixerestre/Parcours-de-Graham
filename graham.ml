

(*mini : ('a -> 'a -> bool) -> 'a list -> 'a
  mini cmp liste : le mini mum de liste, avec cmp
  une fonction de comparaison.*)
let mini cmp liste =
  (* min' : 'a -> 'a list -> 'a
  Le mini mum entre r et le mini mun de liste.*)
  let rec min' r liste = match liste with
    |[] -> r
    |t :: q -> if (cmp t r < 0) then min' t q else min' r q
  in match liste with
    |[] -> failwith "Liste vide."
    |t :: q -> min' t q
;;

let compare_hauteur (x, y) (x', y') =
  if y > y' then 1
  else if y < y' then -1
  else compare x x'
;;

(*determinant : int * int -> int * int -> int * int -> int
  determinant O A B : le déterminant de vecteurs OA et OB.
  positif si OA est à droite par rapport à OB ;
  négatif si OA est à gauche par rapport à OB.*)
let determinant (xa, ya) (xb, yb) (xc, yc) =
  (xb - xa) * (yc - ya) - (yb - ya) * (xc - xa)
;;

let norme_carre (x, y) (x', y') =
  let dx = x - x' in
  let dy = y - y' in
  dx * dx + dy * dy
;;

(* compare_angle : int * int -> int * int -> int * int -> int
  compare_angle O A B : compare si OA est plus à droite que OB, ou
  si les deux sont colinéaires et que ||OA|| > ||OB||*)
let compare_angle o a b =
  let det = determinant o a b in
  if det < 0 then 1
  else if det > 0 then -1
  else compare (norme_carre o a) (norme_carre o b)
;;

(*graham int * int list -> int * int list
  graham l : la liste des sommets de l'enveloppe convexe des
points de l, en partant du point le plus bas.*)
let graham liste =
  if liste = [] then [] else
  let p = mini compare_hauteur liste in
  let sliste = List.sort (compare_angle1 p) liste in
  (*add_point : int * int list -> int * int -> int * int list
    add_point evlp p -> si evlp est une enveloppe convexe de points
    p1, ..., pn et que p est à droite de tous ces points, alors renvoie
    l'enveloppe convexe des points p1, ..., pn, p.*)
  let rec add_point evlp p = match evlp with
    |b :: a :: q ->
      if determinant a p b >= 0 then add_point (a :: q) p else p :: evlp
      (*S'il y a un tour à droite, alors on retire le dernier point de
      l'enveloppe et on continue la recherche. Sinon, on rajoute p à
      l'enveloppe.*)
    |_ -> p :: evlp
  in
  List.fold_left add_point [] sliste
  (*On part d'une enveloppe vide, et on ajoute les points de sliste un par un.*)
;;

(*Tests :*)
let print_liste liste =
  let rec string_of_intlist = function
    |[] -> "Nil"
    |[a, b] -> "(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")"
    |(a, b) :: q ->  "(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")"
^ " " ^ (string_of_intlist q)
  in
  print_endline (string_of_intlist liste)
;;

print_liste (graham []);;
print_liste (graham [(0, 0); (0, 1); (1, 0); (1, 1)]);;
print_liste (graham [(0, 0); (9, 1); (7, 3); (5, 4); (2, 6);
                                (0, 12)]);;

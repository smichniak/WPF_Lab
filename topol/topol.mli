(** Sortowanie topologiczne *)

exception Cykliczne
(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)

val topol : ('a * 'a list) list -> 'a list
(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)


(* Sortowanie topologiczne polega na rozszerzeniu skończonego częściowego porządku do porządku liniowego.
  Mówiąc prościej, mając dany DAG (czyli graf skierowany bez cykli) należy przypisać wierzchołkom takie
  różne liczby naturalne (nadające kolejność tym wierzchołkom), żeby dla każdej krawędzi grafu jej źródło
  miało niższy numer niż jej cel. Mówiąc jeszcze prościej, mając daną częściową informację o zależności,
  powiedzmy, czynności od siebie (np. buty wkładamy po skarpetkach, krawat po koszuli itp. ale kolejność
  wkładania skarpetek i koszuli może być dowolna) mamy wygenerować ścisłą kolejność wykonywania czynności
  (np. koszula, skarpetki, buty, krawat). Konkretnie, należy zaprogramować implementację topol.ml załączonej
  specyfikacji topol.mli.

  W implementacji można korzystać z modułu pMap (bardzo podobnego do pSet z poprzedniego zadania), którego
  specyfikacja i implementacja również są załączone.

*)

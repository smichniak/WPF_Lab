open PMap
exception Cykliczne
(* Możliwe stany wierzchołka podczas przeszukiwania *)
type odwiedzony = Nie | Wszedlem | Wyszedlem

(* Wartości w mapie mają postać odwiedzony*'b list *)
(* Zwraca wierzchołki, do których można bezpośrednio przejść z danego wierzchołka *)
let sasiedzi wierzcholek mapa =
  find wierzcholek mapa |> snd

(* Tworzy mapę wierzchołków do ich list sąsiedztwa *)
let mapa lista =
  let f acc x =
    let elem = fst x in
    let nowa_lista = ref (snd x) in
    if mem elem acc then (* Jeśli wierzchołek jest już w mapie, to łaczy starą i nową listę sąsiadów *)
      nowa_lista := !nowa_lista @ (sasiedzi elem acc);
    add elem (Nie, !nowa_lista) acc in

  List.fold_left f empty lista

let topol lista =
  (* Mapa wierzchołków do list sąsiedztwa *)
  let m = ref (mapa lista) in

  (* Zwraca stan odwiedzenia wierzchołka *)
  let odwiedzony wierzcholek =
    find wierzcholek !m |> fst in

  let wynik = ref [] in

  (* Rozpoczyna przejście w wierzchołku x *)
  let idz x =
    let stos = ref [] in
    begin
      stos := x :: !stos;
      while not (!stos = []) do
        let gora = !stos |> List.hd in
        let nastepni = sasiedzi gora !m in
        (* Stan odwiedzenia wierzchołka na górze stosu *)
        match odwiedzony gora with
        (* Jeśli wcześniej już wyszedłem z danego wierzchołka, to go usuwam *)
        | Wyszedlem -> stos := !stos |> List.tl
        | Wszedlem ->
          (* Jeśli na stosie jest wierzchołek, do którego wcześniej wszedłem, to chcę z niego wyjść *)
          m := add gora (Wyszedlem, nastepni) !m;
          (* Dodaję wierzchołek do wyniku i usuwam ze stosu *)
          wynik := gora::(!wynik);
          stos := !stos |> List.tl
        | _ ->
          (* Jeśli nie byłem wcześniej w wierzchołku, to do niego wchodzę *)
          m := add gora (Wszedlem, nastepni) !m;
          (* Przechodzę po wszyskich wierzchołkach, do których mogę dojść bezpośrednio z aktualnego *)
          List.iter
            (fun y ->
               if not (mem y !m) then
                 (* Jeśli nie ma go w mapie, to znaczy, że nie ma sąsiadów i nie byłem w nim wcześniej *)
                 m := add y (Nie, []) !m;
               match odwiedzony y with
               (* Nieodwiedzony wierzchołek dodaję do stosu *)
               | Nie -> stos := y :: !stos;
               (* Krawędź w górę drzewa rozpinającego, cykl *)
               | Wszedlem -> raise Cykliczne
               (* Wierzchołek już dodany do wyniku *)
               | _ -> () )
            nastepni
      done
    end
  in

  let f a =
    let wierzcholek = fst a in
    (* Rozpoczynam przejście w danym wierzchołku, jeśli nie był odwiedzony *)
    if odwiedzony wierzcholek = Nie then
      idz wierzcholek in

  List.iter f lista;
  !wynik


(* Autor: Szymon Michniak *)
(* Code review: Jakub Szulc *)

(* Testy: *)
(* exception WA;;

let test graph order =
  let hashtbl = Hashtbl.create (List.length order)
  in
  List.iteri (fun i x -> Hashtbl.add hashtbl x i) order;
  let check_one (v, l) =
    List.iter (fun u ->
      if (Hashtbl.find hashtbl v) > (Hashtbl.find hashtbl u)
      then raise WA;) l
  in
  try (List.iter check_one graph; true)
  with WA -> false

let test_cyclic g =
  try let _ = topol g in false
  with Cykliczne -> true

let g = [
  ("1", ["2"; "3"]);
  ("3", ["2"]);
  ("4", ["3"; "2"])
];;
assert(test g (topol g));;

let g = [
  ("first", ["second"; "fourth"; "eighth"]);
  ("second", ["fourth"; "eighth"]);
  ("third", ["fourth"; "fifth"; "sixth"]);
  ("fourth", ["eighth"]);
  ("fifth", ["sixth"; "seventh"]);
  ("sixth", ["eighth"; "first"]);
  ("seventh", ["eighth"]);
];;
assert(test g (topol g));;

let g = [
  (1, [2; 3]);
  (2, [4]);
  (3, [4]);
  (4, [5; 6]);
  (5, [7]);
  (6, [7]);
];;
assert(test g (topol g));;

let g = [
  (1, [7; 2]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;
assert(test g (topol g));;

let g = [
  (1, [2; 4; 8]);
  (2, [16; 32]);
  (4, [64; 128]);
  (8, [256; 512]);
  (16, [1024]);
  (32, [2048]);
  (64, [4096]);
  (128, [8192]);
  (256, [16384]);
  (512, [32768]);
];;
assert(test g (topol g));;

let g = [
  ("Lorem", ["sit"]);
  ("ipsum", ["sit"; "amet"]);
  ("dolor", ["amet"; "elit"]);
  ("sit", ["consectetur"; "adipiscing"; "elit"]);
];;
assert(test g (topol g));;

let g = [];;
assert(test g (topol g));;

let g = [
  ("through", ["the"; "gates"; "of"; "hell"]);
  ("hell", ["as"; "we"; "make"; "our"; "way"; "to"; "heaven"]);
  ("PRIMO", ["VICTORIA"]);
];;
assert(test g (topol g));;

let g = [
  ("one", ["three"]);
  ("one", ["two"]);
  ("two", []);
  ("two", []);
  ("two", ["three"]);
];;
   assert(test g (topol g));;

let g = [
  (10.001, [10.002]);
  (10.002, [10.001])
];;
assert(test_cyclic g);;

let g = [
  (1, [7; 2; 3]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;
assert(test_cyclic g);;

let g = [
  (1, [2]);
  (2, [3]);
  (3, [4; 5; 3]);
];;
assert(test_cyclic g);;

let g = [
  ("pole", ["pole"; "lyse"; "pole"])
];;
assert(test_cyclic g);;

let g = [
  ("tu", ["tudu"]);
  ("tudu", ["tudu"; "tudu"; "tudu"])
];;
assert(test_cyclic g);; *)

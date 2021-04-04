(* Kolejka jako drzewo lewicowe *)
(* Leaf - koniec danej gałęzi *)
(* Node - (wartość, wysokość prawego (minimalnego) poddrzewa, lewe poddrzewo, lewe poddrzewo) *)
type 'a queue = Node of 'a * int * 'a queue * 'a queue | Leaf

(* Puste drzewo lewicowe *)
let empty = Leaf

(* Wyjątek zwracany, gdy chcemy usunąć element z pustej kolejki *)
exception Empty

(* True jeśli kolejka jest pusta, false jeśli nie *)
let is_empty qu =
  match qu with
  | Leaf -> true
  | _ -> false

(* Przyjmujemy, że wysokość liścia = -1, dla drzewa niepustego zwraca wysokość prawgo poddrzewa *)
let min_height qu =
  match qu with
  | Leaf -> -1
  | Node(_, a, _, _) -> a

(* Zmienia drzewo nielewicowe w lewicowe *)
let fix_leftis x =
  match x with
  | (Node(value, height, l, r)) ->
    (* Jeśli wysokość lewgo poddrzewa jest mniejsza niż prawego, to zamienia je miejscami, wysokość tego drzewa to wysokość krótszego poddrzewa (lewego) + 1 *)
    if min_height l < min_height r then (Node(value, (min_height l) + 1, r, l))
    (* Jeśli wysokość prawego poddrzewa jest mniejsza, to nie zamienia miejscami, wysokość tego drzewa to wysokość krótszego poddrzewa (prawgo) + 1 *)
    else (Node(value, (min_height r) + 1, l, r))
  | Leaf -> x

(* Tworzy drzewo jednoelementowe, długość prawego poddrzewa = 0 *)
let create_qu element =
  Node(element, 0, Leaf, Leaf)

(* Łączy dwa drzewa w jedno *)
let rec join qu1 qu2 =
  match qu1, qu2 with
  (* Połączenie liścia i danego drzewa daje to drzewo *)
  | Leaf, _ -> qu2
  | _, Leaf -> qu1
  | Node(value1, height1, l1, r1), Node(value2, height2, l2, r2) ->
    (* Chcemy, żeby pierwsze drzewo miało mniejszą wartość w korzeniu *)
    if value2 < value1 then join qu2 qu1
    (* Korzeniem nowego drzewa będzie korzeń pierwszego drzewa (ma ono mniejszy korzeń, niż drugie), lewym poddrzewem będzie lewe poddrzewo pierwszego drzewa,
       a prawym połączenie prawego poddrzewa i drugiego drzewa *)
    else (Node(value1, (min height1 height2) + 1 ,l1, join r1 qu2)) |> fix_leftis (* Wynikowe drzewo musi być lewicowe *)

(* Dodaje nowy element do drzewa *)
let add element qu =
  (* Tworzy nowe drzewo z jednym elementem i łączy je z drugim drzewem *)
  join (create_qu element) qu

let delete_min qu =
  match qu with
  (* Dla pustego drzewa zwraca wyjątek *)
  | Leaf -> raise Empty
  (* Dla drzewa nieopustego zwraca korzeń i połączenie lewgo i prawgo poddrzewa *)
  | Node(value, min_height, l, r) -> (value, join l r)

(* Autor: Szymon Michniak *)
(* Code review: Marek Masiak *)



(* (* simple tests *)
   let a = empty;;
   let b = add 1 empty;;

   assert (is_empty a = true);;
   assert (try let _=delete_min a in false with Empty -> true);;
   assert (is_empty b <> true);;

   let b = join a b ;;
   assert (is_empty b <> true);;

   let (x,y) = delete_min b;;

   assert (x = 1);;
   assert (is_empty y = true);;
   assert (try let _=delete_min y in false with Empty -> true);;

   (* delete_min integer tests *)
   let b = add 1 empty;;
   let b = add 3 b;;
   let b = add (-1) b;;
   let b = add 2 b;;
   let b = add 1 b;;

   let (a,b) = delete_min b;;
   assert (a = -1);;

   let (a,b) = delete_min b;;
   assert (a = 1);;

   let (a,b) = delete_min b;;
   assert (a = 1);;

   let (a,b) = delete_min b;;
   assert (a = 2);;

   let (a,b) = delete_min b;;
   assert (a = 3);;

   assert(is_empty b = true);;

   (* delete_min string tests *)
   let b = add "a" empty;;
   let b = add "aca" b;;
   let b = add "nzbzad" b;;
   let b = add "nzbza" b;;
   let b = add "bxbxc" b;;

   let (a,b) = delete_min b;;
   assert (a = "a");;

   let (a,b) = delete_min b;;
   assert (a = "aca");;

   let (a,b) = delete_min b;;
   assert (a = "bxbxc");;

   let (a,b) = delete_min b;;
   assert (a = "nzbza");;

   let (a,b) = delete_min b;;
   assert (a = "nzbzad");;

   assert(is_empty b = true);;
   assert (try let _=delete_min b in false with Empty -> true);;

   (* join tests *)

   let b = add 1 empty;;
   let b = add 3 b;;
   let b = add (-1) b;;
   let b = add 2 b;;
   let b = add 1 b;;

   let c = add 10 empty;;
   let c = add (-5) c;;
   let c = add 1 c;;
   let c = add 4 c;;
   let c = add 0 c;;

   let b = join b c;;

   let (a,b) = delete_min b;;
   assert (a = (-5));;

   let (a,b) = delete_min b;;
   assert (a = (-1));;

   let (a,b) = delete_min b;;
   assert (a = 0);;

   let (a,b) = delete_min b;;
   assert (a = 1);;

   let (a,b) = delete_min b;;
   assert (a = 1);;

   let (a,b) = delete_min b;;
   assert (a = 1);;

   let (a,b) = delete_min b;;
   assert (a = 2);;

   let (a,b) = delete_min b;;
   assert (a = 3);;

   let (a,b) = delete_min b;;
   assert (a = 4);;

   let (a,b) = delete_min b;;
   assert (a = 10);;

   assert (try let _=delete_min b in false with Empty -> true);;
*)

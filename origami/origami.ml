type point = float * float
type kartka = point -> int

(* Możliwe położenie punktu względem prostej *)
type polozenie_typ = Lewo | Na | Prawo

let epsilon = 10e-9

let int_of_bool b = if b then 1 else 0

(* Działania na floatach *)
let kwadrat x = x *. x
let podwoj_mnoz x y = 2. *. x *. y

(* Zwraca odbicie punktu (x, y) względem prostej o danych współczynnikach *)
(* Prosta postaci ax + by + c = 0 *)
let odbij (x, y) (a, b, c)  =
  let s_kwadrat_ab = kwadrat a +. kwadrat b in
  let r_kwadrat_ab = kwadrat a -. kwadrat b in
  let p_mnoz_ab = podwoj_mnoz a b in
  let x1 = (x *. (-. r_kwadrat_ab) -. (y *. p_mnoz_ab) -. podwoj_mnoz a c) /. s_kwadrat_ab and
    y1 = (y *. (r_kwadrat_ab) -. (x *. p_mnoz_ab) -. podwoj_mnoz b c) /. s_kwadrat_ab in
  (x1, y1)

(* Zwraca współczynniki prostej przechodzącej przez dwa punkty *)
let prosta (x1, y1) (x2, y2) =
  let a = y2 -. y1 and
  b = x1 -. x2 in
  let c = (a *. x1) +. (b *. y1) in
  (a, b, -.c)

(* Zwraca wartość iloczynu wektorowego [p1, p2] x [p1, punkt], gdzie punkt = (x, y) *)
let iloczyn_wektorowy (p1x, p1y) (p2x, p2y) (x, y) =
  (p2x -. p1x) *. (y -. p1y) -. (p2y -. p1y) *. (x -. p1x)

(* Położenie punktu względem prostej, prostą rysujemy od pierwszego punktu do drugiego *)
let polozenie p1 p2 punkt =
  (* Znak iloczynu wektorowego wyznacza połeżnie punktu względem wektora [p1, p2] *)
  let iloczny = iloczyn_wektorowy p1 p2 punkt in
  if abs_float iloczny < epsilon then Na
  else if iloczny > 0. then Lewo
  else Prawo

(* Zwraca odleglość dwóch punktów o podanych współrzędnych *)
let odleglosc (x1, y1) (x2, y2) =
  hypot (x1 -. x2) (y1 -. y2)

(* Zwraca kartkę, reprezentującą prostokąt o bokach równoległych do osi
   układu współrzędnych i lewym dolnym rogu [p1] a prawym górnym [p2] *)
let prostokat (x1, y1) (x2, y2) (px, py)= int_of_bool
    (* Sprawdza, czy punkty jest pomiędzy wierzchołkami prostokąta *)
    (px <= x2 && px >= x1 && py <= y2 && py >= y1)

(* Zwraca kartkę, reprezentującą kóło o środku w punkcie [srodek] i promieniu [r] *)
let kolko srodek r punkt = int_of_bool (odleglosc srodek punkt <= r)

(* Zwraca kartkę złożoną wzdłuż prostej przechodzącej przez punkty [p1] [p2] *)
let zloz p1 p2 k punkt =
  match polozenie p1 p2 punkt with
  (* Na lewo tyle warstw co przed złożeniem + tyle warstw ile w odbitym punkcie przed złożeniem *)
  | Lewo -> k punkt + k (odbij punkt (prosta p1 p2))
  (* Na prostej tyle samo warstwo co przed złożeniem *)
  | Na -> k punkt
  (* Na prawo od prostej 0 warstw *)
  | Prawo -> 0

(* Zwraca zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...),
   wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych z list *)
let skladaj l k = List.fold_left (fun k (p1, p2) -> zloz p1 p2 k) k l

(* Autor: Szymon Michniak *)
(* Code review: Jakub Moliński *)

(* let centr = (0., 0.);;
   let info = false;;
   let a = prostokat centr (10., 10.);;
   assert(a centr = 1);;
   assert(a (5., 5.) = 1);;
   assert(a (10., 10.) = 1);;
   assert(a (10., 0.) = 1);;
   assert(a (0., 10.) = 1);;
   assert(a (10.1, 0.) = 0);;
   assert(a (0., 10.1) = 0);;
   assert(a (10.1, 10.1) = 0);;
   let a = zloz (5., 0.) (5., 377.) a;;
   assert(a centr = 2);;
   assert(a (-377., 0.) = 0);;
   assert(a (5., 2.5) = 1);;
   assert(a (2.5, 3.5) = 2);;
   assert(a (5., 5.) = 1);;
   assert(a (5.1, 5.) = 0);;
   assert(a (5.1, 5.1) = 0);;
   let a = zloz (5., 0.) (5., 1.) a;;
   assert(a centr = 2);;
   assert(a (-377., 0.) = 0);;
   assert(a (5., 2.5) = 1);;
   assert(a (2.5, 3.5) = 2);;
   assert(a (5., 5.) = 1);;
   assert(a (5.1, 5.) = 0);;
   assert(a (5.1, 5.1) = 0);;
   let a = kolko (3., 3.) 7.;;
   assert(a centr = 1);;
   assert(a (3., 3.) = 1);;
   assert(a (8., 7.5) = 1);;
   assert(a (10., 3.) = 1);;
   assert(a (3., 10.) = 1);;
   assert(a (-4., 3.) = 1);;
   assert(a (3., -4.) = 1);;
   assert(a (10.1, 3.) = 0);;
   assert(a (10., 3.1) = 0);;
   assert(a (-4.1, 3.) = 0);;
   assert(a (-3.9, 3.) = 1);;
   let a = zloz (5., -10.) (5., 100.) a;;
   assert(a centr = 1);;
   assert(a (0.67, 0.) = 1);;
   assert(a (0.68, 0.) = 2);;
   assert(a (0.69, 0.69) = 2);;
   assert(a (1., 0.) = 2);;
   assert(a (2., 2.) = 2);;
   assert(a (3., 0.) = 2);;
   assert(a (5., 0.) = 1);;
   assert(a (5.1, 0.) = 0);;
   assert(a (3., 3.) = 2);;
   assert(a (3., 10.) = 1);;
   assert(a (-1., -1.) = 1);;
   assert(a (7., 7.) = 0);;
   assert(a (10., 0.) = 0);;    *)

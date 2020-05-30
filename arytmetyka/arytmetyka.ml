type wartosc = float * float
(* Wartość jest parą floatów *)
(* Niech: type(a) = type(b) = float i a < b*)
(* Wartość dokładana od a ma postać (a, a) *)
(* Zbiory typu [a, b] mają postać (a, b) *)
(* Zbiory typu (-inf, a] u [b, inf) mają postać (b, a) *)
(* Zbiory typu [a, inf) mają postać (a, neg_infinity) *)
(* Zbiory typu (-inf, a] mają postać (infinity, a) *)

let check_nan (first, second) =
  if ((not (first = first)) || (not (second = second))) then true
  else false

let wartosc_dokladnosc x p = (* Dla x >= 0, (x * (1- p%), x * (1 + p%)) i odwrotnie dla x < 0 *)
  let first = x *. ((100. +. p) /. 100.) and
  second = x *. ((100. -. p) /. 100.)
  in if first <= second then (first, second)
  else (second, first)

let wartosc_od_do x y =
  (x, y)

let wartosc_dokladna x =
  (x, x)

let in_wartosc (first, second) x =
  if first <= second then (x >= first) && (x <= second) (* Dla zbioru [a, b] (ograniczonego), a <= x <= b *)
  else (x >= first) || (x <= second) (* Dla zbioru niogranicznego odwrotnie *)

let min_wartosc (first, second) =
  if first <= second then first (* Dla zbioru ograniczonego, mniejszy z końców *)
  else match (first, classify_float second) with
    | (_, FP_nan) -> nan
    | (a, FP_infinite) -> a (* Dla zbioru [a, infinity], a *)
    | _ -> neg_infinity (* Zbiory niograniczone z dołu *)

let max_wartosc (first, second) =
  if first <= second then second (* Dla zbioru ograniczonego, większy z końców *)
  else match (classify_float first, second) with
    | (FP_nan, _) -> nan
    | (FP_infinite, a) -> a (* Dla zbioru [neg_infinity, a], a *)
    | _ -> infinity (* Zbiory niograniczone z góry *)

let sr_wartosc wartosc = (* (a+b) / 2 dla zbioru [a,b] i infinity lub neg_infinity dla zbiórów nieograniczonych *)
  ((min_wartosc wartosc) +. (max_wartosc wartosc)) /. 2.

let przeciwny (first, second) =
  (-.second, -.first)

let plus ((first1, second1) as war1) ((first2, second2) as war2) =
  if (check_nan war1) || (check_nan war2) then (nan, nan)

  else if (first1 <= second1) && (first2 <= second2) then (first1 +. first2, second1 +. second2) (* Dwa zbiory ograniczone, plus [a, b] [c, d] = [a+b, c+d] *)
  else (first1 +. first2, second1 +. second2) (* Conajmniej jeden zbiór jest nieograniczonych *)
       |> fun (x, y) -> if ((x = infinity) && (y = neg_infinity) || x <= y) then (neg_infinity, infinity)
       else (x, y)
(* Jeśli wynkiem dodawania zbiorów nieograniczonych jest zbiór ograniczony, to końce przedziałów się "zazębiają" i wynikiem jest R *)

let minus war1 war2 = (* Odejmowanie to dodawanie zbioru przeciwnego *)
  plus war1 (przeciwny war2)

let (#*) x y = (* neg_infinity #* 0. = 0. = infinity #* 0. *)
  if ((x = 0.) || (y = 0.)) then 0.
  else x *. y

let iloczyn (first1, second1) (first2, second2) = (* Posortowana niemalejąco lista czterech wartości*)
  List.sort compare [first1 #* first2; first1 #* second2; second1 #* first2;second1 #* second2]

let minmax war1 war2 = (* Zbiór ograniczony z najmniejszej i najwięszkej wartości iloczynu czterech końców przedziałów *)
  (List.nth (iloczyn war1 war2) 0, List.nth (iloczyn war1 war2) 3)

let srodkowe war1 war2 = (* Zbiór nieograniczony z środkowych wartości iloczynu czterech końców przedziałów *)
  (List.nth (iloczyn war1 war2) 2, List.nth (iloczyn war1 war2) 1)

let infnite (first1, second1) (first2, second2) =
  (* Mnożenie przez zbiór jednostronnie ograniczny *)
  if classify_float second2 = FP_infinite && first2 > 0. then (0., neg_infinity)
  else if classify_float second2 = FP_infinite then (second1 #* first2, neg_infinity)
  else if classify_float first2 = FP_infinite && second2 < 0. then (infinity, 0.)
  else if classify_float first2 = FP_infinite then (first1 #* first2, second1 #* second2)

  else (neg_infinity, infinity) (* Zbiór nieograniczny *)

let rec razy ((first1, second1) as war1) ((first2, second2) as war2)=
  if (check_nan war1) || (check_nan war2) then (nan, nan)

  else if war1 = wartosc_dokladna 0. || war2 = wartosc_dokladna 0. then (0., 0.) (* Dowolny przedział * [0, 0] = [0, 0] *)

  else if first1 <= second1 && first2 <= second2 then minmax war1 war2 (* Zbiory ograniczone [a, b], [c,d], minimum i maksimum z iloczynów a, b, c, d *)

  else (if ((not (in_wartosc war1 0.)) && (not (in_wartosc war2 0.))) then srodkowe war1 war2 (* Conajmniej jeden zbiór nieograniczony i oba bez 0 *)
        (* Jeśli któryś ze zbiorów zawiera tylko lewostronne otoczenie zera, to zmieniamy je na prawostronne *)
        else if (second1 = 0.) then przeciwny (razy (przeciwny war1) war2)
        else if (second2 = 0.) then przeciwny (razy war1 (przeciwny war2))
        else if (not (in_wartosc war1 0.) && (in_wartosc war2 0.)) then razy war2 war1 (* W pierwszym na pewno będzie 0, bo w conajmniej jednym z nich jest 0 *)
        else if (first1 = 0.) then (* Pierwszy zbiór ma prawostronne otoczenie 0 *)
          if first1 <= second1 then (* [0, b] * coś  *)
            infnite war1 war2
          else if classify_float second1 = FP_infinite then (* [a, inf) * coś gdzie a <= 0*)
            if first2 <= second2 then
              if first2 >= 0. then (0., neg_infinity) (* [a, inf) * [b, c] = [0, inf), b >= 0, c > b  *)
              else if first2 < 0. && second2 > 0. then (neg_infinity, infinity) (* [a, inf] * [b, c] = (-inf, inf), b < 0 < c *)
              else (infinity, 0.) (* [a, inf) * [b, c] = (-inf, 0], b, c < 0 *)
            else infnite war1 war2 (* [a, inf) * zbiór nieograniczny *)
          else if first2 <= second2 then (* (-inf, a] u [b, inf) * [b, c] *)
            if first2 > 0. then (first1 #* second2, second1 #* first2) (* Zbiór [b, c] nie zawiera 0 *)
            else if second2 > 0. then (neg_infinity, infinity) (* Zbiór [b, c] zawiera obustronne oteczenie 0 *)
            else (second2 #* second1, 0.)  (* Zbiór [b, c] zawiera jednostronne otoczenie 0 *)
          else if classify_float second2 = FP_infinite then (* (-inf, a] u [b, inf) * [b, inf) *)
            if first2 > 0. then (first1 #* second2, second1 #* first2) (* [b, inf) nie zaweiera 0 *)
            else (neg_infinity, infinity) (* Zbiór [b, inf) zawiera obustronne otoczenie 0 *)
          else if classify_float first2 = FP_infinite then (* (-inf, a] u [b, inf) * (-inf, b] *)
            if second2 < 0. then (second1 #* second2, first1 #* first2) (* (-inf, b] nie zaweiera 0 *)
            else (neg_infinity, infinity) (* Zbiór (-inf, b] zawiera obustronne otoczenie 0 *)
          else (neg_infinity, infinity) (* (-inf, a] u [b, inf) * (-inf, c] u [d, inf) *)
        else if (first2 > second2) then (neg_infinity, infinity) (* Pierwszy ma obustronne otecznie 0 a drugi jest nieograniczony *)
        else  srodkowe war1 war2 (* Pierwszy ma obustronne otecznie 0 *)

              |> fun (x, y) -> if ((x = infinity) && (y = neg_infinity) || x <= y) then (neg_infinity, infinity)
              else (x, y) ) (* Jeśli wynkiem mnożenia zbioru nieograniczonego jest zbiór ograniczony, to końce przedziałów się "zazębiają" i wynikiem jest R *)

let odwrotny ((first, second) as war) =
  if war = wartosc_dokladna 0. then (nan, nan) (* Odwrotny do [0,0] *)
  else if (first = 0. && second > first) then (1. /. second, neg_infinity) (* Odwrotny do [0, a] = [1/a, infinity) *)
  else if (second = 0. && second > first) then (infinity, 1. /. first) (* Odwrotny do [a, 0] = (neg_infinity, 1/a] *)
  else if (first = neg_infinity && second = infinity) then war (* Odwrotny do R = R *)

  else (1. /. second, 1. /. first) (* Dla pozostałych zbiorów *)

let podzielic war1 war2 = (* Dzielenie to mnożenie przez zbiór odwrotny *)
  razy war1 (odwrotny war2)

(* Szymon Michniak *)
(* Code review: Miłosz Piekutowski *)


(* Testy *)
(*let ( =. ) (x : float) (y : float) =
  let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
  let a = min_wartosc ( podzielic ( wartosc_od_do (0.000000) (0.000000) ) ( wartosc_od_do (-9.800000) (-2.000000) ) ) ;;
  assert (a =. 0.);;
  let a = min_wartosc ( podzielic ( wartosc_od_do (-2.600000) (9.800000) ) ( wartosc_od_do (0.000000) (0.000000) ) ) ;;
  assert ((classify_float a) == FP_nan);;
  let a = max_wartosc ( podzielic ( wartosc_dokladnosc (4.800000) (1.800000) ) ( wartosc_dokladnosc (1.600000) (1.000000) ) ) ;;
  assert (a =. 3.08484848484848451);;
  let a = max_wartosc ( podzielic ( wartosc_od_do (-0.600000) (0.400000) ) ( wartosc_od_do (-9.000000) (4.800000) ) ) ;;
  assert (a = infinity);;
  let a = sr_wartosc ( razy ( wartosc_od_do (0.000000) (5.200000) ) ( wartosc_dokladnosc (2.200000) (5.000000) ) ) ;;
  assert (a =. 6.006);;
  let a = sr_wartosc ( plus ( wartosc_dokladnosc (4.000000) (7.800000) ) ( podzielic ( wartosc_od_do (-8.600000) (0.400000) ) ( wartosc_dokladna (-1.600000) ) ) ) ;;
  assert (a =. 6.56250000000000089);;
  let a = in_wartosc ( podzielic ( wartosc_dokladnosc (6.200000) (9.400000) ) ( wartosc_dokladna (3.400000) ) ) (-8.600000);;
  assert (a = false);;
  let a = sr_wartosc ( plus ( wartosc_dokladna (0.000000) ) ( wartosc_dokladnosc (7.600000) (1.600000) ) ) ;;
  assert (a =. 7.6);;
  let a = min_wartosc ( plus ( podzielic ( wartosc_dokladna (0.000000) ) ( wartosc_dokladnosc (0.000000) (9.000000) ) ) ( wartosc_dokladna (7.600000) ) ) ;;
  assert ((classify_float a) == FP_nan);;
  let a = sr_wartosc ( plus ( wartosc_od_do (0.000000) (3.400000) ) ( wartosc_dokladnosc (-5.600000) (6.200000) ) ) ;;
  assert (a =. -3.89999999999999947);;
  let a = in_wartosc ( minus ( podzielic ( wartosc_od_do (-1.200000) (0.000000) ) ( minus ( wartosc_dokladnosc (3.400000) (1.400000) ) ( razy ( wartosc_od_do (-7.200000) (0.400000) ) ( wartosc_dokladnosc (-0.800000) (1.200000) ) ) ) ) ( wartosc_dokladnosc (-4.600000) (5.600000) ) ) (8.000000);;
  assert (a = true);;
  let a = sr_wartosc ( minus ( podzielic ( podzielic ( minus ( wartosc_od_do (0.000000) (0.000000) ) ( wartosc_od_do (-0.400000) (5.200000) ) ) ( wartosc_od_do (-8.400000) (-3.400000) ) ) ( podzielic ( wartosc_od_do (-4.600000) (-3.800000) ) ( wartosc_dokladnosc (7.000000) (1.200000) ) ) ) ( wartosc_dokladnosc (0.000000) (9.000000) ) ) ;;
  assert (a =. -1.31591331269349876);;
  let a = max_wartosc ( minus ( minus ( wartosc_dokladna (8.400000) ) ( wartosc_od_do (-1.000000) (2.200000) ) ) ( podzielic ( wartosc_dokladna (0.000000) ) ( wartosc_dokladna (-4.000000) ) ) ) ;;
  assert (a =. 9.4);;
  let a = max_wartosc ( podzielic ( podzielic ( wartosc_dokladnosc (-8.200000) (1.200000) ) ( wartosc_dokladnosc (4.600000) (3.200000) ) ) ( wartosc_dokladnosc (-5.400000) (4.200000) ) ) ;;
  assert (a =. 0.360248272565600214);;
  let a = sr_wartosc ( podzielic ( minus ( wartosc_od_do (8.000000) (9.600000) ) ( wartosc_dokladna (-5.800000) ) ) ( wartosc_od_do (-5.800000) (0.000000) ) ) ;;
  assert (a = neg_infinity);;
  let a = min_wartosc ( podzielic ( wartosc_dokladna (0.000000) ) ( wartosc_dokladnosc (5.200000) (1.000000) ) ) ;;
  assert (a =. 0.);;
  let a = max_wartosc ( plus ( podzielic ( wartosc_dokladnosc (0.000000) (2.000000) ) ( plus ( wartosc_dokladna (3.600000) ) ( wartosc_od_do (1.600000) (9.400000) ) ) ) ( wartosc_od_do (-2.000000) (0.000000) ) ) ;;
  assert (a =. 0.);;
  let a = in_wartosc ( podzielic ( wartosc_od_do (-2.000000) (0.000000) ) ( minus ( wartosc_dokladnosc (0.800000) (9.200000) ) ( wartosc_dokladnosc (-2.400000) (8.600000) ) ) ) (-4.200000);;
  assert (a = false);;
  let a = sr_wartosc ( podzielic ( wartosc_dokladnosc (-7.000000) (9.800000) ) ( razy ( wartosc_dokladna (-5.800000) ) ( wartosc_od_do (2.200000) (7.200000) ) ) ) ;;
  assert (a =. 0.376774207593173149);;
  let a = sr_wartosc ( podzielic ( podzielic ( wartosc_dokladna (0.000000) ) ( minus ( wartosc_od_do (0.000000) (6.200000) ) ( minus ( wartosc_od_do (0.000000) (0.000000) ) ( wartosc_dokladnosc (0.000000) (5.800000) ) ) ) ) ( wartosc_od_do (-1.600000) (0.600000) ) ) ;;
  assert (a =. 0.);;
  let a = sr_wartosc ( podzielic ( wartosc_od_do (-1.800000) (0.000000) ) ( wartosc_od_do (-1.000000) (0.000000) ) ) ;;
  assert (a = infinity);;
  let a = in_wartosc ( razy ( wartosc_od_do (-0.400000) (5.000000) ) ( wartosc_od_do (-3.200000) (0.000000) ) ) (8.600000);;
  assert (a = false);;
  let a = min_wartosc ( minus ( wartosc_od_do (-6.600000) (0.000000) ) ( plus ( wartosc_dokladna (-2.000000) ) ( minus ( wartosc_od_do (-4.400000) (-4.400000) ) ( wartosc_dokladna (0.000000) ) ) ) ) ;;
  assert (a =. -0.199999999999999289);;
  let a = sr_wartosc ( plus ( wartosc_dokladnosc (-9.000000) (0.000000) ) ( razy ( minus ( plus ( wartosc_dokladnosc (-7.600000) (0.000000) ) ( minus ( wartosc_dokladna (-7.200000) ) ( plus ( wartosc_dokladnosc (8.000000) (1.600000) ) ( podzielic ( wartosc_dokladna (8.800000) ) ( wartosc_dokladna (-10.000000) ) ) ) ) ) ( wartosc_dokladnosc (1.200000) (0.000000) ) ) ( minus ( wartosc_dokladna (0.000000) ) ( wartosc_dokladna (4.000000) ) ) ) ) ;;
  assert (a =. 83.48);;
  let a = sr_wartosc ( podzielic ( razy ( wartosc_dokladnosc (0.600000) (8.800000) ) ( razy ( wartosc_dokladnosc (1.000000) (0.000000) ) ( wartosc_dokladnosc (8.000000) (0.000000) ) ) ) ( podzielic ( wartosc_dokladna (-1.400000) ) ( wartosc_od_do (1.200000) (3.000000) ) ) ) ;;
  assert (a =. -7.47154285714285749);;
  let a = sr_wartosc ( razy ( wartosc_dokladnosc (-9.400000) (6.800000) ) ( wartosc_od_do (0.000000) (0.000000) ) ) ;;
       assert (a =. 0.);;
*)

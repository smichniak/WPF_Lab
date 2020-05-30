open Array

let rec nwd a b =
  if b = 0 then a
  else nwd b (a mod b)

(* Liczy NWD (x1, x2, ..., xn) tablicy [|(x1, y1); (x2, y2); ...; (xn, yn)|] *)
let nwd_tablicy tab =
  fold_left (fun acc (x, _) -> nwd acc x) 0 tab

(* Sprawdza, czy stan końcowy jest poprawny *)
let da_sie tab =
  (* Na końcu conajmniej jedna szklanka musi być pusta albo pełna *)
  let pelna_pusta = exists (fun (x, y) -> y = x || y = 0) tab in
  let nwdx = nwd_tablicy tab in
  (* Każdy z poziomów końcowych musi dzielić NWD wszystkich pojemności *)
  let dzieli = nwdx = 0 || for_all (fun (_, y) -> y mod nwdx = 0) tab in
  pelna_pusta && dzieli

let przelewanka szklanki =
  let n = length szklanki in
  if n = 0 then 0 else
  if da_sie szklanki |> not then -1 else
  let odwiedzeni = Hashtbl.create 10000 in
  let wynik = ref (-1) in
  let kolejka = Queue.create () in
  let koncowy = map snd szklanki in (* Tablica z poziomami końcowymi *)
  let pojemnosci = map fst szklanki in (* Tablica pojemności szklanek *)
  let poziomy = make n 0 in (* Zaczynamy z pustymi szklankami *)

  let sprawdz_stan stan kroki =
    (* Rozważam tylko stany, których jeszcze nie odwiedziłem *)
    if Hashtbl.mem odwiedzeni stan |> not then (
      Queue.push (kroki, stan) kolejka; (* Wkładam stan do kolejki bfs *)
      Hashtbl.add odwiedzeni stan (); (* Zaznaczam, że odwiedziłem stan *)
      if stan = koncowy then wynik := kroki) in (* Sprawdzam, czy stan jest wynikiem *)

  (* Generuje nowe stany, wykonując operacje na danej szklance *)
  let nowe_stany stan szklanka kroki =
    let poziom = stan.(szklanka) in
    let pojemnosc = pojemnosci.(szklanka) in
    if pojemnosc <> 0 then ( (* Nie rozważam szklanek o pojemnosci 0 *)
      if poziom = pojemnosc then ( (* Wylewam wodę z pełnych szklanek *)
        let wylej = copy stan in
        wylej.(szklanka) <- 0;
        sprawdz_stan wylej (kroki+1));

      if poziom <> 0 then ( (* Przelewam wodę z niepustych szklanek *)
        for szklanka2 = 0 to (n-1) do
          let poziom2 =  stan.(szklanka2) in
          let pojemnosc2 = pojemnosci.(szklanka2) in
          (* Nie przelewam wody do tej samej szklanki i pełnych szklanek *)
          if szklanka2 <> szklanka && poziom2 <> pojemnosc2 then (
            let przelej = copy stan in
            przelej.(szklanka2) <- min pojemnosc2 (poziom + poziom2);
            przelej.(szklanka) <- max 0 (poziom + poziom2 - pojemnosc2);
            sprawdz_stan przelej (kroki+1))
        done)

      else ( (* poziom = 0, nalewam wodę do pustych szklanek *)
        let nalej = copy stan in
        nalej.(szklanka) <- pojemnosc;
        sprawdz_stan nalej (kroki+1))) in

  sprawdz_stan poziomy 0; (* Sprawdzam stan początkowy *)

  (* Sprawdzam tak długo, aż nie odwiedzę wszystkich stanów, albo dojdę do wyniku *)
  while Queue.is_empty kolejka |> not && !wynik = -1 do
    let (kroki, stan) = Queue.pop kolejka in
    for i = 0 to n-1 do (* Generuję nowy stan dla każdej szklanki *)
      nowe_stany stan i kroki
    done
  done;
  !wynik


(* Autor: Szymon Michniak *)
(* Code review: Dominik Wisniewski *)
(* assert (przelewanka [| (10,2); (1,1) |] = 5);;
   assert (przelewanka [| (0,0); (2,2); (2,2); (2,2); (0,0); (0,0); (1,0);
   (0,0); (1,0) |] = (3));;
   assert (przelewanka [| (1,1); (2,1); (3,0); (4,2) |] = (3));;
   assert (przelewanka [| (0,0); (2,2); (1,0); (1,1); (1,0); (2,2); (1,0);
   (0,0); (0,0) |] = (3));;
   assert (przelewanka [| (11,11); (11,1) |] = (-1));;
   assert (przelewanka [| (1,1); (0,0); (2,2); (0,0); (2,0); (0,0); (0,0);
   (1,0); (2,0); (1,0) |] = (2));;
   assert (przelewanka [| (5,2); (0,0); (0,0); (2,0); (3,2) |] = (4));;
   assert (przelewanka [| (1,1); (0,0); (4,4); (4,0); (4,4) |] = (3));;
   assert (przelewanka [| (9,9); (13,12) |] = (10));;
   assert (przelewanka [| (2,2); (1,0); (2,2); (0,0); (1,0); (0,0); (1,1);
   (1,0); (0,0) |] = (3));;
   assert (przelewanka [| (5,2); (3,1); (0,0); (4,1); (0,0); (1,0) |] = (5));;
   assert (przelewanka [| (310,76); (139,91) |] = (-1));;
   assert (przelewanka [| (48,9); (12,0); (1,1); (65,64) |] = (10));;
   assert (przelewanka [| (7,5); (3,3); (9,4); (10,4); (6,3); (5,3) |] =
   (8));;
   assert (przelewanka [| (100000,50000); (1,1) |] = (100000));;
   assert (przelewanka [| (0,0); (0,0); (0,0); (300000,151515);
   (1,0); (0,0) |] = (296971));;
   assert (przelewanka [| (11,2); (11,10); (4,0); (10,8); (21,16) |] = (12));;
   assert (przelewanka [| (50,1); (7,3); (78,64) |] = (-1));;
   assert (przelewanka [| (85,23); (524,210) |] = (-1));;
   assert (przelewanka [| (557,349); (73,49) |] = (-1));;
   assert (przelewanka [| (62,3); (38,7) |] = (-1));;
   assert (przelewanka [| (15,15); (6,3); (42,32); (33,20) |] = (-1));;
   assert (przelewanka [| (39,12); (35,34); (21,7); (2,1) |] = (-1));;
   assert (przelewanka [| (1,0); (2,1); (2,1); (0,0); (2,0); (0,0); (0,0);
   (0,0); (1,1); (0,0); (1,0) |] = (4));;
   assert (przelewanka [| (2,0); (2,2); (2,1); (6,6); (0,0) |] = (-1));;
   assert (przelewanka [| (2,0); (1,1); (1,1); (1,1); (0,0); (1,0); (3,2);
  (0,0) |] = (4));;
   assert (przelewanka [| (1,1); (2,2); (4,1); (0,0); (1,0); (2,1) |] = (5));;
   assert (przelewanka [| (1,0); (3,1); (2,2); (1,1); (1,0); (1,0) |] = (3));;
   assert (przelewanka [| (20,7); (12,11) |] = (-1));;
   assert (przelewanka [| (0,0); (21,21) |] = (1));;
   assert (przelewanka [| (13,8); (11,11) |] = (14));;
   assert (przelewanka [| (1,1); (3,2); (6,5) |] = (5));;
   assert (przelewanka [| (4,4); (7,6); (2,2) |] = (6));;
   assert (przelewanka [| (3,2); (3,3); (1,1); (2,0) |] = (3));;
   assert (przelewanka [| (0,0); (2,0); (0,0); (2,0); (3,2); (2,1); (1,0) |] =
   (3));;
*)

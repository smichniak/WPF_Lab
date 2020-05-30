(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz, Szymon Michniak
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Interval Set.

    This is an interval set, i.e. a set of integers, where large
    intervals can be stored as single elements. Intervals stored in the
    set are disjoint.

*)

(* Integer interval [a, b], a <= b *)
type interval = int * int

(* Integer set in form of an AVL tree. Nodes are in form:
   left subtree, given interval, right subtree, tree hight,
   (number of elements in left subtree, number of elements in right subtree) *)
type t =
  | Empty
  | Node of t * interval * t * int * (int * int)

(* Possible outcomes of interval comparing *)
type cmp_result = Left | Right | Overlap

(* Check overflow during addition. If a+b < 0 return max_int otherwise a+b *)
let (#+) x y =
  let sum = x + y in
  if sum < 0 then max_int
  else sum

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* Empty set *)
let empty = Empty

let is_empty x =
  x = Empty

(* Singleton of number x *)
let singleton x = (x, x)

(* [iter f s] applies [f] to all continuous intervals in the set [s].
    The intervals are passed to [f] in increasing order. *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set

(* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are all continuous intervals of s, in increasing order. *)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
      loop (f k (loop acc l)) r in
  loop acc set

(* Return the list of all continuous intervals of the given set.
    The returned list is sorted in increasing order. *)
let elements set =
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set

(* Returns the number of integers in a given interval *)
let interval_len = function
    (first, second) -> second #+ (-first) #+ 1

(* Returns the number of integers in a given tree *)
let rec total_below =
  function
  | Empty -> 0
  (* Sum of interval in node and left and right subtrees *)
  | Node (_, k, _, _, (l_below, r_below)) -> l_below #+ r_below #+ (interval_len k)

(* Creates a pair of lengths for left and right subtree *)
let below_pair = function
    l, r -> (total_below l, total_below r)

(* Creates a tree with given subtress and value in Node *)
let make l k r = Node (l, k, r, max (height l) (height r) + 1, below_pair (l, r))

(* Creates a balanced AVL tree with given subtrees and value in Node *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then make ll lk (make lr k r)
      else
        (match lr with
         | Node (lrl, lrk, lrr, _, _) ->
           make (make ll lk lrl) lrk (make lrr k r)
         | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then make (make l k rl) rk rr
      else
        (match rl with
         | Node (rll, rlk, rlr, _, _) ->
           make (make l k rll) rlk (make rlr rk rr)
         | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* Compares two intervals *)
let cmp (first1, second1) (first2, second2) =
  if second1 < first2 then Left
  else if first1 > second2 then Right
  else Overlap (* Intervals have at least one common element *)

(* [mem x s] returns [true] if [s] contains [x], and [false] otherwise. *)
let mem x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
      let c = cmp (singleton x) k in
      (c <> Left && c <> Right) || loop (if c = Left then l else r)
    | Empty -> false in
  loop set

(* Returns interval with smallest maximal value *)
let rec min_interval = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_interval l
  | Empty -> raise Not_found

(* Returns a balanced tree without minimal interval *)
let rec remove_min_interval = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_interval l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(* Adds an interval to the set that has no common elements with it *)
let rec add_one x = function
  | Node (l, k, r, h, bel) -> (
      match cmp x k with
      | Left ->
        let nl = add_one x l in
        bal nl k r
      | _ ->
        let nr = add_one x r in
        bal l k nr )
  | Empty -> make Empty x Empty

(* Creates a balanced tree using given interval and two trees *)
let rec join l v r =
  match (l, r) with
  |  (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, lb), Node(rl, rv, rr, rh, rb)) ->
    if lh > rh + 2 then bal ll lv (join lr v r) else
    if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* Splits interval in a given place (x) and adds resulting subintervals to corresponding trees.
   Left subinterval to left tree and right subinterval to right tree*)
let split_help (first, second) x l_tree r_tree =
  let l =
    if first < x
    then add_one (first, min second (x - 1)) l_tree
    else l_tree
  and r =
    if second > x
    then add_one (max first (x + 1), second) r_tree
    else r_tree
  in
  (l, true, r)

(* [split x s] returns a triple [(l, present, r)], where
    [l] is the set of elements of [s] that are strictly lesser than [x];
    [r] is the set of elements of [s] that are strictly greater than [x];
    [present] is [false] if [s] contains no element equal to [x],
    or [true] if [s] contains an element equal to [x]. *)
let split x set =
  let rec loop x = function
      Empty ->
      (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
      match cmp (singleton x) v with
      | Overlap -> split_help v x l r
      | Left -> let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
      | _ -> let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
  (setl, pres, setr)

(* [remove (x, y) s] returns a set containing the same elements as [s],
    except for all those which are included between [x] and [y].
    Assumes [x <= y]. *)
let remove (first, second) set =
  (* Splits the set in bounds of the interval *)
  let (l, _, temp_r) = split first set in
  let (_, _, r) = split second temp_r in
  match r with
  | Empty -> l
  (* Joins trees to the left and right of the removed interval *)
  | Node _ -> join l (min_interval r) (remove_min_interval r)

let int_of_bool b = if b then 1 else 0

(* [below n s] returns the number of elements of [s] that are lesser
    or equal to [n]. If there are more than max_int such elements,
    the result should be max_int. *)
let below x set =
  (* Splits the set in x and returns the number of integers in left part of the split,
     if x was in the set, 1 is added to the sum *)
  let (l_split,is_in ,_) = split x set in
  (total_below l_split) #+ (int_of_bool is_in)

(* Increase number by 1 unless it's a max_int *)
let increase x = if x = max_int then max_int else x + 1

(* Decrease number by 1 unless it's a min_int *)
let decrease x = if x = min_int then min_int else x - 1

(* Finds max value of interval after merging it with all overlapping intervals in the tree *)
let rec max_second ((first, second) as interv) = function
  | Node (l, k, r, _, _) ->(
      match cmp (decrease first, increase second) k with
      | Left ->
        max_second interv l
      | Right ->
        max_second interv r
      | _ ->
        if snd interv <= snd k
        then snd k
        else max_second interv r )
  | Empty ->
    snd interv

(* Finds min value of interval after merging it with all overlapping intervals in the tree *)
let rec min_first ((first, second) as interv) = function
  | Node (l, k, r, _, _) ->(
      match cmp (decrease first, increase second) k with
      | Left ->
        min_first interv l
      | Right ->
        min_first interv r
      | _ ->
        if fst interv >= fst k
        then fst k
        else min_first interv l)
  | Empty ->
    fst interv

(* Returns interval after merging it with every overlapping interval in the set *)
let merge_intrev interv set =
  let min_val = min_first interv set
  and max_val = max_second interv set in
  (min_val, max_val)


(* [add (x, y) s] returns a set containing the same elements as [s], plus all
    elements of the interval [[x,y]] including [x] and [y]. Assumes [x <= y]. *)
let add interval set =
  let merged_interval = merge_intrev interval set in
  let set_remove = remove merged_interval set in
  add_one merged_interval set_remove


(* let zle = ref 0
   let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Zly wynik testu %d!\n" id;
        incr zle
    end;;

   open ISet;;



   let s = empty;;
   test 11 (is_empty s) true;;
   test 12 (is_empty (add (1, 1) s)) false;;


   (* niestety musimy zalozyc poprawnosc mem... *)

   let s = add (10, 12) empty;;
   test 21 (mem 9 s) false;;
   test 22 (mem 10 s) true;;
   test 23 (mem 12 s) true;;
   test 24 (mem 13 s) false;;

   let s = add (4, 7) s;;
   test 25 (mem 8 s) false;;
   test 26 (mem 11 s) true;;
   test 27 (mem 5 s) true;;
   test 28 (mem 3 s) false;;


   let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
   test 31 (mem 10 (remove (10, 10) s)) false;;
   test 32 (is_empty (remove (1, 20) s)) true;;
   test 33 (mem 7 (remove (8, 15) s)) true;;

   let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
   let s = remove (-10, 12) s;;
   test 34 (is_empty s) false;;
   test 35 (mem 5 s) false;;
   test 36 (mem (-10) s) false;;
   test 37 (mem (-15) s) true;;
   test 38 (mem 17 s) true;;


   test 41 (elements (add (4, 5) (add (7, 8) empty)) = [(4, 5); (7, 8)]) true;;
   test 42 (elements (add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty))))
        = [(1, 1); (4, 9); (11, 14)]) true;;


   let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
   test 51 (below 2 s = 0) true;;
   test 52 (below 3 s = 1) true;;
   test 53 (below 10 s = 5) true;;
   test 54 (below 15 s = 6) true;;
   test 55 (below 100 s = 11) true;;
   let s = add (1, max_int) (add (-1, 0) empty);;
   test 56 (below max_int s = max_int) true;;
   let s = add (-min_int, max_int) empty;;
   test 57 (below max_int s = max_int) true;;
   test 58 (below min_int s = 1) true;;


   let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
   let l, pres, r = split 9 s;;
   test 61 (mem 9 l) false;;
   test 62 (mem 9 r) false;;
   test 63 (mem 8 l) true;;
   test 64 (mem 10 r) true;;
   test 65 pres true;;
   test 66 (mem 7 l) false;;
   test 67 (mem 4 l) true;;
   test 68 (mem 11 r) false;;
   test 69 (mem 16 r) true;;


   let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
   let a = ref [];;
   let foo x = a := x::!a; ();;
   test 71 (iter foo s; !a = [(11, 14); (4, 9); (1, 1)]) true;;


   let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
   let foo x a = x::a;;
   test 81 (fold foo s [] = [(11, 14); (4, 9); (1, 1)]) true;; *)

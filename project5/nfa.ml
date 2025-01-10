open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

(* Self-made Method*)
let count_occ lst target = List.fold_left (fun acc element -> if element = target then acc + 1 else acc) 0 lst

(* Self-made Method*)
let uniq lst = List.fold_left (fun acc element -> if count_occ acc element = 0 then acc @ [element] else acc) [] lst

(* Self-made Method*)
let rec state_transition (nfa_delta: ('q, 's) transition list) (q: 'q) (s: 's option) : 'q list =
  match nfa_delta with
  | [] -> []
  | (q1, s1, q2) :: t -> if q1 = q && s1 = s then [q2] @ (state_transition t q s) else (state_transition t q s)

let rec move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  match qs with
  | [] -> []
  | h :: t -> uniq ((state_transition nfa.delta h s) @ (move nfa t s))

(* Self-made Method*)
let rec state_transition_inclusive (nfa_delta: ('q, 's) transition list) (q: 'q) (s: 's option) : 'q list =
  match nfa_delta with
  | [] -> [q]
  | (q1, s1, q2) :: t -> if q1 = q && s1 = s then [q2] @ (state_transition_inclusive t q s) else (state_transition_inclusive t q s)

(* Self-made Method*)
let rec interal_e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) (visited: 'q list): 'q list =
  match qs with
  | [] -> []
  | h :: t -> let states = state_transition_inclusive nfa.delta h None 
  in 
  let worker = minus (uniq (t @ states)) visited
  in
  uniq (states  @ (interal_e_closure nfa worker (h :: visited)))


let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let a = interal_e_closure nfa qs []
  in 
  if List.length a = List.length qs then qs else e_closure nfa a

(* Self-made Method*)
let apply_char acc c = 
  match acc with
  | (nfa, states) -> let a = move nfa states (Some c) in (nfa, e_closure nfa a)

(* Self-made Method*)
let rec contains_endpoints nfa lst = match lst with
| [] -> false
| h :: t -> if List.mem h nfa.fs then true else contains_endpoints nfa t

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let char_list = explode s 
  in 
  let starting_states = e_closure nfa [nfa.q0]
  in 
  let acc = (nfa, starting_states) 
  in 
  let (_, end_states) = fold_left apply_char acc char_list 
in contains_endpoints nfa end_states


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let apply_char_to_state acc state = match acc with 
  | (nfa, states, c) -> let (_, new_states) = apply_char (nfa, [state]) c in (nfa, uniq (states @ new_states), c)

let new_states1 (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let new_states_for_char s = 
    let (_, states, _) = List.fold_left (apply_char_to_state) (nfa, [], s) qs in states
  in
  List.map new_states_for_char nfa.sigma

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let new_states_for_char s = e_closure nfa (move nfa qs (Some s)) in List.map new_states_for_char nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let new_trans_for_char s = (qs, Some s, e_closure nfa (move nfa qs (Some s))) in List.map new_trans_for_char nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if contains_endpoints nfa qs then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (visited: 'q list list): ('q list, 's) nfa_t =
  let new_states_list h = new_states nfa h
  in
  let rec new_final_list list = match list with 
    | [] -> []
    | h :: t -> uniq((new_finals nfa h) @ new_final_list t)
  in
  let work = minus dfa.qs visited 
  in
  match work with
  | [] -> dfa
  | h :: t -> let temp_states_list = new_states_list h 
  in let new_dfa = { 
    sigma = dfa.sigma;
    qs = uniq(dfa.qs @ temp_states_list);
    q0 = dfa.q0;
    fs = uniq((dfa.fs @ new_final_list temp_states_list));
    delta = uniq(dfa.delta @ new_trans nfa h);
  } in nfa_to_dfa_step nfa new_dfa (visited @ [h])
  

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let new_dfa_q0 = interal_e_closure nfa [nfa.q0] []
  in 
  let new_dfa = {
    sigma = nfa.sigma;
    qs = [new_dfa_q0];
    q0 = new_dfa_q0;
    fs = [] @ new_finals nfa new_dfa_q0;
    delta = [];
  }
  in
  nfa_to_dfa_step nfa new_dfa []


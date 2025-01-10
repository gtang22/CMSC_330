open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with
| (a, b, c) -> (c, b, a)

(* Check absolute value? *)
let is_even x = if x mod 2 = 0 then true else false

let volume x y = match x, y with
| (a, b, c), (d, e, f) -> abs((a - d) * (b - e) * (c - f))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = match n with
| 0 -> 0
| 1 -> 1
| _ -> fibonacci(n - 1) + fibonacci(n - 2)

let rec log x y = match y/x with
| 0 -> 0
| 1 -> 1
| _ -> (log x (y/x)) + 1

let rec gcf x y = match y with
| 0 -> x
| _ -> gcf y (x mod y)

let rec maxFuncChain init funcs = match funcs with
| [] -> init
| h::t -> max (maxFuncChain init t) (maxFuncChain (h init) t)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec reverse lst = match lst with
| [] -> []
| h::t -> reverse t @ [h]

(* Self made method *)
let combine_tuples tup1 tup2 = match tup1, tup2 with
| (a, b), (c, d) -> (a, b, c, d)

let rec zip lst1 lst2 = match lst1, lst2 with
| _, [] -> []
| [], _ -> []
| h1 :: t1, h2 :: t2 -> (combine_tuples h1 h2) :: zip t1 t2


let rec is_palindrome lst = if reverse lst = lst then true else false

(* Self made method *)
let rec is_prime num =
  let rec is_divisible m = m * m > num || (num mod m != 0 && is_divisible (m + 1)) 
in num >= 2 && is_divisible 2

let rec square_primes lst = match lst with
| [] -> []
| h :: t -> if is_prime h then ((h, h * h) :: square_primes t) else square_primes t

(* Self made method *)
let rec filter p list = match list with
| [] -> []
| h :: t -> if p h then (filter p t) else h :: (filter p t)

(* Self made method *)
let rec exclude p list = match list with
| [] -> []
| h :: t -> if p h then h :: (exclude p t) else (exclude p t)


let partition p lst = 
let b = filter p lst
in let a = exclude p lst
in (a, b)

(*****************)
(* Part 4: HOF *)
(*****************)

let is_present lst x = 
  let check_n n = if n = x then 1 else 0 in map check_n lst

let count_occ lst target = fold (fun acc element -> if element = target then acc + 1 else acc) 0 lst

let join a x = match a, x with
| (c, 0), (e, f, g, h) -> (c @ [h], 1)
| (c, _), (e, f, g, h) -> (c @ [e], 0)

let jumping_tuples lst1 lst2 = 
  match fold (join) ([], 0) (zip lst1 lst2), fold (join) ([], 1) (zip lst1 lst2) with
| (a, b), (c, d) -> a @ c

let addgenerator x = fun y -> fold (fun acc element -> acc + element) x [y]

let uniq lst = fold (fun acc element -> if count_occ acc element = 0 then acc @ [element] else acc) [] lst

let ap fns args = fold (fun acc funct -> acc @ map funct args) [] fns 

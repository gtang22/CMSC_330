open LccTypes 
open Str
open String

let cntr = ref (-1)

let fresh () =
  cntr := !cntr + 1 ;
  !cntr

let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then e else lookup t var

let reset () = 
  cntr := 0;
  !cntr

(* Counts the number of times target occures in lst*)
let count_occ lst target = List.fold_left (fun acc element -> if element = target then acc + 1 else acc) 0 lst

(* Makes sure all the elements in lst are unique*)
let uniq lst = List.fold_left (fun acc element -> if (count_occ acc element) = 0 then acc @ [element] else acc) [] lst

(* Sees if s1 contains s2*)
let contains lst x = if (count_occ lst x) = 0 then false else true

(* get ast string presentation *)
let rec string_of_term ast = match ast with
  | Var x -> x
  | Func (x, t) -> "(L" ^ x ^ "." ^ (string_of_term t) ^ ")"
  | Application (t1, t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"

(* Removes var from list*)
let rec remove_var var list = match list with 
  | [] -> []
  | h :: t -> if h = var then remove_var var t else h :: (remove_var var t)

(* get difference list from two lists *)
let difference lst1 lst2 = List.fold_left (fun acc x -> if (count_occ lst2 x) = 0 then x :: acc else acc) [] lst1

(* get new variable name *)
let rec new_variable lst var = if contains lst var then new_variable lst (var ^ "'") else var                         

(* Gets all free variable names *)
let rec free_vars ast var_list = match ast with
  | Var x -> x :: var_list
  | Func (x, y) -> let var_list1 = free_vars y var_list in remove_var x var_list1
  | Application (x, y) -> let var_list1 = free_vars x var_list in 
                        let var_list2 = free_vars y var_list in
                        uniq (var_list1 @ var_list2)

(* get all variable names *)
let rec all_vars ast var_list = match ast with
  | Var x -> x :: var_list
  | Func (x, y) -> x :: (all_vars y var_list) 
  | Application (x, y) -> let var_list1 = all_vars x var_list in 
                        let var_list2 = all_vars y var_list in
                        uniq (var_list1 @ var_list2)

(* get bounded variable names *)
let bounded_vars ast = difference (all_vars ast []) (free_vars ast [])

(* convert variable name x to variable name y in ast *)
let rec replace x y ast = match ast with
  | Var s   -> if s = x then Var y else ast
  | Func (s, t) -> if (s = x) then Func (s, t) else Func (s, replace x y t)
  | Application (t, u) -> Application (replace x y t, replace x y u)

(* get index of variable in list *)
let rec vars_index var_list x idx = match var_list with
  | [] -> string_of_int idx
  | h :: t -> if h = x then string_of_int idx else vars_index t x (idx+1) 

(* convert variable name to int *)
let rec standard_ast ast = let allvars = all_vars ast [] in
    let freevars = free_vars ast [] in
    let boundvars = difference allvars freevars in 
      let rec standard e = (match e with
        | Var x -> if contains boundvars x then Var (new_variable allvars (vars_index allvars x 0)) else e
        | Application (x, y) -> Application(standard x, standard y)
        | Func (x, y) -> Func(new_variable allvars (vars_index allvars x 0), standard y) 
      ) in 
        standard ast

(* alpha conversion *)
let rec alpha_convert e = let a = reset() in
    let rec alpha e = (match e with
      | Var x -> e
      | Application (x, y) -> Application(alpha x, alpha y)
      | Func (x, y) -> let b = string_of_int (fresh()) in Func (b, alpha (replace x b y))) in
      alpha e 

(* is alpha equals *)
let isalpha e1 e2 = 
  let ast1 = alpha_convert e1 in 
    let ast2 = alpha_convert e2 in 
      if ast1 = ast2 then true else false

(* extract value from option *)
let extract o = match o with
  | Some i -> i

(* substitute var with term *)
let rec substitute var term ast = match ast with
  | Var x   -> if x = var then term else ast
  | Func (x, y) -> let fvt = free_vars y [] in
      let fvterm = free_vars term [] in
        if (x = var) then Func (x, y)
        else if (not (contains fvt var)) then Func(x, y)
        else if (contains fvterm x) then
          let f = new_variable (fvt @ fvterm) x in
          Func (f, substitute var term (replace x f y))
        else
          Func (x, substitute var term y)
  | Application (x, y) -> Application (substitute var term x, substitute var term y)

(* apply environment variables *)
let rec apply_env_step env ast = let freevars = free_vars ast [] in 
  let rec apply_env_step_i env ast vars = match vars with
    | [] -> ast
    | h::t -> let x = lookup env h in 
              if x = None then apply_env_step_i env ast t 
              else let i = fresh() in apply_env_step_i env (substitute h (extract x) ast) t in
    apply_env_step_i env ast freevars

(* beta reduce step *)
let rec beta_reduce_step body s arg = match body with
  | Var x -> if x = s then arg else body
  | Application (x, y) -> Application (beta_reduce_step x s arg, beta_reduce_step y s arg)
  | Func (x, y) -> if x = s then substitute x arg y else Func (x, beta_reduce_step y s arg)

(* beta reduce *)
let rec reduce env e = 
  let a = print_endline ("reduce: " ^ string_of_term e) in 
    let ast = alpha_convert e in
      let newast = apply_env_step env ast in
        let rec reduce_first e1 = match e1 with
          | Application (Func(x, body), arg) -> let arg1 = reduce_first arg in reduce_first (beta_reduce_step body x arg1)
          | Application (x, y) -> let ne = Application(reduce_first x, reduce_first y) in if isalpha ne e1 then ne else reduce_first ne
          | Func (x, y) -> Func (x, reduce_first y)
          | Var x -> e1 in
          reduce_first newast

(* check whether execution is allowed *)
let canExecuteStep maxStep = !cntr < maxStep

(* For lazy reduce *)
let rec laze env e = 
  let a = print_endline ("laze: " ^ string_of_term e) in 
    let ast = alpha_convert e in
      let maxsteps = !cntr + 1 in
        let rec laze_reduce ast maxstep = 
          if canExecuteStep maxstep then 
            (match ast with
              | Application (Func(x, body), arg) -> let a = fresh() in substitute x arg body
              | Application (x, y) -> Application (laze_reduce x maxstep, laze_reduce y maxstep)
              | Func (x, y) -> Func (x, laze_reduce y maxstep)
              | Var x -> ast)
          else ast in
            let e1 = laze_reduce e maxsteps in
              if canExecuteStep maxsteps then apply_env_step env e1 else e1

(* for eager reduce *)
let rec eager env e = 
  let a = print_endline ("eager: " ^ string_of_term e) in 
    let ast = alpha_convert e in
      let maxsteps = !cntr + 1 in
        let newast = apply_env_step env ast in
          let rec eager_reduce e1 mstep = 
            if canExecuteStep mstep then
              (match e1 with
                  | Application(Func(x, body), arg) -> let arg1 = eager_reduce arg mstep in 
                      if canExecuteStep mstep then let t = fresh() in substitute x arg1 body else Application(Func(x, body), arg1)
                  | Application (x, y) -> Application (eager_reduce x mstep, eager_reduce y mstep)
                  | Func (x, y) -> Func (x, eager_reduce y mstep)
                  | Var x -> e1)
            else e1 in
              eager_reduce newast maxsteps

let rec convert tree = match tree with 
  | Bool true -> "(Lx.(Ly.x))"
  | Bool false -> "(Lx.(Ly.y))"
  | If (a, b, c) -> "((" ^ convert a ^ " " ^ convert b ^ ") " ^ convert c ^ ")"
  | Not a -> "(" ^ "(Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) " ^ convert a ^ ")"
  | And (a, b) -> "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) " ^ convert a ^ ") " ^ convert b ^ ")"
  | Or (a, b) -> "(((Lx.(Ly.((x (Lx.(Ly.x))) y))) "^ convert a ^ ") " ^ convert b ^ ")"

(* handle true or false *)
let true_false x y z = match z with
  | Var s -> if s = x then "true" else if s = y then "false" else ""
  | _ -> ""

(* convert ast to string *)
let rec readable tree = let ntree = alpha_convert tree in match ntree with
  | Application (Application( Func(x, Func(x', Application(Application(x'', Func(x''', _)), _))), a), b) -> "(" ^ (readable a) ^ " or " ^ (readable b) ^ ")"
  | Application (Application( Func(x, Func(x', Application(Application(x'', _), _))), a), b) -> "(" ^ (readable a) ^ " and " ^ (readable b) ^ ")"
  | Application (Application(x, y), z) -> "(if " ^ (readable x) ^ " then " ^ (readable y) ^ " else " ^ (readable z) ^ ")"
  | Application (Func(x, Application(y, z)), a) ->"(" ^ "not " ^ (readable a) ^ ")"
  | Func (x, Func(y, z)) -> true_false x y z
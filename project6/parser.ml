open LccTypes 

let match_token (toks : 'a list) (tok : 'a) : 'a list =
  match toks with
  | [] -> raise (Failure("List was empty"))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure( 
      Printf.sprintf "Token passed in does not match first token in list"
    ))

let lookahead toks = match toks with
   h::t -> h
  | _ -> raise (Failure("Empty input to lookahead"))



let print_token tok = match tok with
| Lambda_EOF -> print_endline "Lamdba_EOF"
| Lambda_LParen -> print_endline "Lamdba_("
| Lambda_RParen -> print_endline "Lamdba_)"
| Lambda_Lambda -> print_endline "Lamdba_Lambda"
| Lambda_Var x-> print_endline "Lamdba_Var"
| Lambda_Dot -> print_endline "Lamdba_Dot"

let rec print_tokens toks = match toks with
| [] -> print_endline "end"
| h :: t -> let x = print_token h in print_tokens t

(* Write your code below *)

(*
   e -> x
   | (Lx.e)
   | (e e)
  *)
let rec parse_lambda toks = 
  let (t, expression) = parse_e toks in 
  if t <> [Lambda_EOF] then 
    raise (Failure "parsing failed")
  else
    expression

  and parse_e toks =
    let next_tok = lookahead toks in match next_tok with
    | Lambda_Var x -> let new_toks = match_token toks (Lambda_Var x) in
                      (new_toks, Var (x))
    | Lambda_LParen -> let new_toks = match_token toks Lambda_LParen in 
                        match lookahead new_toks with
                        | Lambda_Lambda -> (let newer_toks = match_token new_toks Lambda_Lambda in
                                           match lookahead newer_toks with
                                           | Lambda_Var y -> let t1 = match_token newer_toks (Lambda_Var y) in
                                                             let t2 = match_token t1 Lambda_Dot in  
                                                             let (t3, e) = parse_e t2 in
                                                             let t4 = match_token t3 Lambda_RParen in
                                                             (t4, Func (y, e))
                                                             
                                           | _ -> raise (Failure "parsing failed"))
                        | _ -> let (t1, e1) = parse_e new_toks in 
                               let (t2, e2) = parse_e t1 in 
                               let t3 = match_token t2 Lambda_RParen in 
                               (t3, Application (e1, e2))
    | _ -> raise (Failure "parsing failed")



let rec parse_engl toks = 
  let (t, expression) = parse_C toks in
  if t <> [Engl_EOF] then
   raise (Failure "parsing failed")
  else 
    expression 

  (* Finished parse_C *)
  and parse_C toks = 
    let next_tok = lookahead toks in match next_tok with
    | Engl_If -> let new_toks = match_token toks Engl_If in 
               let (t1, first_C) = parse_C new_toks in
               let t1' = match_token t1 Engl_Then in
               let (t2, second_C) = parse_C t1' in 
               let t2' = match_token t2 Engl_Else in 
               let (t3, third_C) = parse_C t2' in 
               (t3, If (first_C, second_C, third_C))
    | _ -> parse_H toks

  (* Finished parse_H *)
  and parse_H toks = 
    let (t, u) = parse_U toks in
    let next_tok = lookahead t in match next_tok with
    | Engl_And -> let new_toks = match_token t Engl_And in
                  let (t1, h) = parse_H new_toks in
                  (t1, And (u, h))
    | Engl_Or -> let new_toks = match_token t Engl_Or in
                 let (t1, h) = parse_H new_toks in
                 (t1, Or (u, h))
    | _ -> t, u
      
  (* Finished parse_U *)
  and parse_U toks =
    let next_tok = lookahead toks in match next_tok with
    | Engl_Not -> let new_toks = match_token toks Engl_Not in
                  let (t', u) = parse_U new_toks in
                  (t', Not (u))
    | _ -> parse_M toks
    
  (* Finished parse_M *)
  and parse_M toks = 
    let next_tok = lookahead toks in match next_tok with
    | Engl_LParen -> let new_toks = match_token toks Engl_LParen in
                     let (newer_toks, c) = parse_C new_toks in
                     let newest_toks = match_token newer_toks Engl_RParen in
                     (newest_toks, c) 
    | Engl_False -> let new_toks = match_token toks Engl_False in
                    (new_toks, Bool false)
    | Engl_True -> let new_toks = match_token toks Engl_True in
                    (new_toks, Bool true)
    | _ -> raise (Failure "parsing failed")

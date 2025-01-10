open OUnit2
open Lcc.Lexer
open Lcc.Parser
open Lcc.Eval
open Lcc.LccTypes


(* parse_lambda (lex_lambda "((Lx.x) a)");;*)
(* -------------LEXER-------------- *)
let public_lex_lambda _ =
  let test_cases = ["(Lx.x)";
                    "...";
                    "L L.x y.x L)"] in
  let results =  [[Lambda_LParen; Lambda_Lambda; Lambda_Var "x"; Lambda_Dot; Lambda_Var "x"; Lambda_RParen;Lambda_EOF];
                  [Lambda_Dot; Lambda_Dot; Lambda_Dot; Lambda_EOF];
                  [Lambda_Lambda; Lambda_Lambda; Lambda_Dot; Lambda_Var "x"; Lambda_Var "y"; Lambda_Dot; Lambda_Var "x"; Lambda_Lambda; Lambda_RParen; Lambda_EOF]
                 ] in
  assert (general_test "public_lex_lambda" (lex_lambda) test_cases results)

let public_lex_engl _ = 
  let test_cases = ["true and or false";
                    "not  not     not)";
                    "(( if not ) and true false"] in

  let results =  [[Engl_True; Engl_And; Engl_Or; Engl_False; Engl_EOF];
                  [Engl_Not; Engl_Not; Engl_Not; Engl_RParen; Engl_EOF];
                  [Engl_LParen; Engl_LParen; Engl_If; Engl_Not; Engl_RParen; Engl_And; Engl_True; Engl_False; Engl_EOF]
                 ] in
  assert (general_test "public_lex_engl" (lex_engl) test_cases results)




let suite = 
  "public" >::: [
    "public_lex_lambda" >:: public_lex_lambda;
  ]

let _ = run_test_tt_main suite
open LccTypes

let rec lex_lambda input : lambda_token list = 
  let length = String.length input
  in
  let rec get_token_list pos =
    if pos >= length then 
      [Lambda_EOF]
  else if Str.string_match (Str.regexp "(") input pos then
    Lambda_LParen :: (get_token_list (pos + 1))
  else if Str.string_match (Str.regexp ")") input pos then
    Lambda_RParen :: (get_token_list (pos + 1))
  else if Str.string_match (Str.regexp "\.") input pos then
    Lambda_Dot :: (get_token_list (pos + 1))
  else if Str.string_match (Str.regexp "[a-z]") input pos then
    let value = Str.matched_string input 
    in
    Lambda_Var(value) :: (get_token_list (pos + 1))
  else if Str.string_match (Str.regexp "L") input pos then
    Lambda_Lambda :: (get_token_list (pos + 1))
  else if Str.string_match (Str.regexp " ") input pos then 
    (get_token_list (pos + 1))
  else
    raise (Failure "tokenizing failed")

  in get_token_list 0



let rec lex_engl input : engl_token list = 
  let length = String.length input
  in
  let rec get_token_list pos =
    if pos >= length then 
      [Engl_EOF]
  else if Str.string_match (Str.regexp "(") input pos then
    Engl_LParen :: get_token_list (pos + 1)
  else if Str.string_match (Str.regexp ")") input pos then
    Engl_RParen :: get_token_list (pos + 1)
  else if Str.string_match (Str.regexp "true") input pos then
    Engl_True :: get_token_list (pos + 4)
  else if Str.string_match (Str.regexp "false") input pos then
    Engl_False :: get_token_list (pos + 5)
  else if Str.string_match (Str.regexp "if") input pos then
    Engl_If :: get_token_list (pos + 2)
  else if Str.string_match (Str.regexp "then") input pos then
    Engl_Then :: get_token_list (pos + 4)
  else if Str.string_match (Str.regexp "else") input pos then
    Engl_Else :: get_token_list (pos + 4)
  else if Str.string_match (Str.regexp "and") input pos then
    Engl_And :: get_token_list (pos + 3)
  else if Str.string_match (Str.regexp "or") input pos then
    Engl_Or :: get_token_list (pos + 2)
  else if Str.string_match (Str.regexp "not") input pos then
    Engl_Not :: get_token_list (pos + 3)
  else if Str.string_match (Str.regexp " ") input pos then 
    (get_token_list (pos + 1))
  else 
    raise (Failure "tokenizing failed")
    
  in get_token_list 0

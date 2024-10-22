
open Utils

let lex s = (* We take in a string which we end up tokenizing*)
  let words = split s in (* We split the string into words using the split from the Utils*)
  let token_options = List.map tok_of_string_opt words in (* List.map here is applying tok_of_string_opt to each word in the split array I believe*)
  let rec collect_tokens opts = (*This function is used to collect the tokens recursively*)
    match opts with (*pattern match to build out the function*)
    | [] -> Some [] 
    | None :: _ -> None 
    | Some tok :: rest ->
        match collect_tokens rest with
        | None -> None
        | Some toks -> Some (tok :: toks)
  in
  match collect_tokens token_options with
  | None -> None
  | Some toks -> Some toks


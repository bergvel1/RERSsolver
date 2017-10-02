{
	 (*#use "tokens.ml";;*)
   open Parse
   exception Eof
}

let numeric     = ['0' - '9']
let lowercase   = ['a' - 'z']
let letter      = ['a' - 'z' 'A' - 'Z' '_']
let ident_char = letter | numeric | '_' | '\''
let string_char = ident_char | ' ' | '~' | '`' | '!' | '@' | '#' | '$' | '%' | '^' | '&'
  | '*' | '(' | ')' | '-' | '+' | '=' | '{' | '[' | '}' | ']'
  | '|' | ':' | ';' | '<' | ',' | '>' | '.' | '?' | '/' 
let line_comment = ('#'|"//")[^'\n']*'\n'

rule token = parse
    | [' ' '\t' '\n']   { token lexbuf }  (* skip over whitespace *)
    | line_comment    { token lexbuf }    
    | "extern void __VERIFIER_error(int);"  { token lexbuf }
    | "return -2;"   { CRASH (-2) }
    | "scanf(\"%d\", &input);"  { INPUT }
    | "while"     { WHILE }
    | "int input;"  { token lexbuf }
    | ("void " (ident_char+) "("(ident_char*)");")  { token lexbuf }
    | "fprintf(stderr, \"Invalid input: %d\\n\", input);" { INPUTERROR }
    | "printf(\"%d\\n\", " (numeric+ as o) "); fflush(stdout);"  { OUTPUT (int_of_string o) }
    | "__VERIFIER_error("(numeric+ as c)");"  { CRASH (int_of_string c) }
    | eof               { EOF }
  	| "+" 				{ PLUS } 
  	| "-"				{ MINUS }
  	| "*"				{ TIMES }
    | "/"       { DIV }
  	| "%"				{ MOD }
  	| "<"				{ LT }
  	| ">"				{ GT }
  	| "<="				{ LEQ }
  	| ">="				{ GEQ }
  	| "=="				{ EQ }
  	| "!="				{ NEQ }
  	| ";"				{ SEMI }
    | ":"       { COLON }
    | "?"       { TERN }
  	| "="				{ ASSIGN }
    | "+="      { PLUSASSIGN }
    | "-="      { MINUSASSIGN }
  	| "if"				{ IF }
  	| "else" 			{ ELSE} 
  	| "!"				{ NEG }
  	| "&&"				{ LOGICALAND }
  	| "||"				{ LOGICALOR }
  	| "["				{ LBRAC }
  	| "]"				{ RBRAC }
  	| "("				{ LPAREN }
  	| ")"				{ RPAREN }
  	| "{"				{ LBRACE }
  	| "}"				{ RBRACE }
  	| ","				{ COMMA }
  	| "void"			{ VOIDTYPE }
  	| "int"				{ INTTYPE }
  	| numeric+ as s { INT (int_of_string s) }
  	| (ident_char*) as s { IDENT s }
    | ((ident_char* as s)"[]") { IDENT s }
    | ('&' (ident_char* as s)) { REFIDENT s }
    | ('*' (ident_char* as s)) { DEREFIDENT s }
 	| "\""   { string "" lexbuf }           
and string ins = parse
    | string_char+ as s { string (ins ^ s) lexbuf }
    | "\\n" { string (ins ^ "\n") lexbuf }
    | "\""  { STRING ins }


{

(* do not modify this function: *)

let lextest s = token (Lexing.from_string (s^"\n"))


let get_all_tokens s =
  let c = open_in s in
  let b = Lexing.from_channel c in
  let rec g () toks=
    match token b with
        EOF -> toks
      | t ->  g () (t :: toks)
  in
  let res = g () [] in
  let _ = close_in c in
  res

let get_all_token_options s =
    let c = open_in s in
    let b = Lexing.from_channel c in
    let rec g () toks = (*let _ = print_string "." in *)
        match (try (Some (token b),b.lex_curr_p) with _ -> (None,b.lex_curr_p)) with
            (Some EOF,p) -> (Some EOF,p)::toks
            | (None,p) -> (None,p)::toks
            | (Some t,p) -> g () ((Some t,p) :: toks)
        in
    let res = g () [] in
    let _ = close_in c in
    res

let get_n_token_options s n =
    let c = open_in s in
    let b = Lexing.from_channel c in
    let rec g toks n =
        if (n = 0) then (Some EOF,b.lex_curr_p)::toks else
        match (try (Some (token b),b.lex_curr_p) with _ -> (None,b.lex_curr_p)) with
            (Some EOF,p) -> (Some EOF,p)::toks
            | (None,p) -> [(None,p)]
            | t -> g (t :: toks) (n-1)
        in
    let res = g [] n in
    let _ = close_in c in
    res

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)


}

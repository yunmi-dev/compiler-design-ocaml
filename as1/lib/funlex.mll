(* CSE322 Compiler Assignment 1 *)
{
exception Eof

(* create a tuple of the start and end positions *)
let make_pos buf = (Lexing.lexeme_start buf, Lexing.lexeme_end buf)

(* track comment nesting depth *)
let comment_depth = ref 0
}

let digit = ['0'-'9']
let alpha = ['A'-'Z''a'-'z']
let id = (alpha)(alpha|digit|'_')*
let whitespace = [' ' '\t' '\r']
let number = digit+

rule initial = parse
  (* reserved keywords *)
  | "fun"  { Tokens.tok_fun(make_pos lexbuf) }
  | "in"   { Tokens.tok_in(make_pos lexbuf) }
  | "let"  { Tokens.tok_let(make_pos lexbuf) }
  | "while" { Tokens.tok_while(make_pos lexbuf) }
  | "do"   { Tokens.tok_do(make_pos lexbuf) }
  | "if"   { Tokens.tok_if(make_pos lexbuf) }
  | "then" { Tokens.tok_then(make_pos lexbuf) }
  | "else" { Tokens.tok_else(make_pos lexbuf) }
  | "ref"  { Tokens.tok_ref(make_pos lexbuf) }
  | "not"  { Tokens.tok_not(make_pos lexbuf) }

  (* operators and delimiters - longer ones first *)
  | "->"   { Tokens.tok_arrow(make_pos lexbuf) }
  | ":="   { Tokens.tok_assign(make_pos lexbuf) }
  | "||"   { Tokens.tok_or(make_pos lexbuf) }
  | "!"    { Tokens.tok_bang(make_pos lexbuf) }
  | "&"    { Tokens.tok_and(make_pos lexbuf) }
  | ">"    { Tokens.tok_gt(make_pos lexbuf) }
  | "<"    { Tokens.tok_lt(make_pos lexbuf) }
  | "="    { Tokens.tok_eq(make_pos lexbuf) }
  | "("    { Tokens.tok_lparen(make_pos lexbuf) }
  | ")"    { Tokens.tok_rparen(make_pos lexbuf) }
  | "+"    { Tokens.tok_plus(make_pos lexbuf) }
  | "-"    { Tokens.tok_minus(make_pos lexbuf) }
  | "*"    { Tokens.tok_times(make_pos lexbuf) }
  | ":"    { Tokens.tok_colon(make_pos lexbuf) }
  | ";"    { Tokens.tok_semicolon(make_pos lexbuf) }
  | ","    { Tokens.tok_comma(make_pos lexbuf) }

  (* projection operator *)
  | "#" (digit+ as num) { 
      if String.length num > 1 && num.[0] = '0' then
        Errormsg.impossible ("Invalid projection: #" ^ num ^ " (leading zeros not allowed)")
      else
        Tokens.tok_proj(make_pos lexbuf, int_of_string num)
    }

  (* comments *)
  | "/*" { 
      comment_depth := 1; 
      comment lexbuf; 
      initial lexbuf 
    }
  
  (* identifiers and numbers *)
  | id { Tokens.tok_id(make_pos lexbuf, Lexing.lexeme lexbuf) }
  | number { Tokens.tok_num(make_pos lexbuf, int_of_string(Lexing.lexeme lexbuf)) }
  
  (* whitespace *)
  | whitespace { initial lexbuf }
  | "\n" { Errormsg.new_line(make_pos lexbuf); initial lexbuf }
  
  (* end of file *)
  | eof { raise Eof }
  
  (* illegal characters *)
  | _ { Errormsg.impossible ("Illegal character: " ^ Lexing.lexeme lexbuf) }

  | "\n"   { Errormsg.new_line(make_pos lexbuf); initial lexbuf }
  | eof    { raise Eof }


(* comment handling rule *)
and comment = parse
  | "*/" { 
      comment_depth := !comment_depth - 1;
      if !comment_depth = 0 then ()
      else comment lexbuf 
    }
  | "/*" { 
      comment_depth := !comment_depth + 1; 
      comment lexbuf 
    }
  | "\n" { Errormsg.new_line(make_pos lexbuf); comment lexbuf }
  | eof { Errormsg.impossible "Comment not closed" }
  | _ { comment lexbuf }
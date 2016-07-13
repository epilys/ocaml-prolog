(*
Copyright 2016 Emmanouil Pitsidianakis

This file is part of ocaml-prolog.

ocaml-prolog is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

ocaml-prolog is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ocaml-prolog.  If not, see <http://www.gnu.org/licenses/>.
*)
{
open Lexing
open Parser

    exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1;
}
}

let whitespace = [ ' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let constant = ['a' - 'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let variable = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse 
    | whitespace { token lexbuf }
    | newline { next_line lexbuf ; token lexbuf }
    | variable as v { VAR v }
    | constant as v { CONST v }
    | '(' { LPAREN  }
    | ')' { RPAREN }
    | ':' { COLON  }
    | '-' { DASH }
    | '.' { PERIOD  }
    | ',' { COMMA }
    | '[' { LBRACKET }
    | ']' { RBRACKET }
    | '\'' { APOSTROPHE }
    | eof { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

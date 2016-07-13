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
%{
    open Prolog
%}

%token <string> CONST
%token <string> VAR
%token COMMA
%token COLON
%token PERIOD
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token EOF
%token DASH
%token APOSTROPHE

%start main
%type <Prolog.clause list> main

%%

main:
    | EOF { [] }
    | clause { $1 }

clause:
    | rule main { $1 :: $2 }

rule:
    | LBRACKET v = VAR RBRACKET PERIOD 
    | LBRACKET v = CONST RBRACKET PERIOD { { head = (Op ("assert",[(Var v)])); body=[] } }
    | CONST PERIOD { { head=(Op ($1,[])); body=[] } }
    | CONST LPAREN arguments RPAREN PERIOD { { head = (Op ($1,$3)); body = [] } }
    | CONST LPAREN arguments RPAREN COLON DASH goals PERIOD { { head=(Op ($1,$3)) ; body= $7 } }

goals:
    | goal { [$1] }
    | goal COMMA goals { $1 :: $3 }

goal:
    | CONST LPAREN arguments RPAREN { Op ($1,$3) } 

arguments:
    | term { [$1] }
    | term COMMA arguments { $1 :: $3 }

term:
    | APOSTROPHE CONST APOSTROPHE { Const $2 } 
    | CONST { Const $1 }
    | VAR { Var $1 }
    | goal { $1 }
%%

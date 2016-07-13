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
open Core.Std
open Lexer
open Lexing
open Prolog
open Print

let print_position outx lexbuf =
      let pos = lexbuf.lex_curr_p in
        fprintf outx "%s:%d:%d" pos.pos_fname
            pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
            
let parse lexbuf = 
    try Parser.main Lexer.token lexbuf with 
  | SyntaxError msg ->
          print_string msg ; []
  | Parser.Error ->
          fprintf stdout "%a: syntax error\n" print_position lexbuf;  []

          (*
let rec print_parsed l =
    let rec loop x =
        match x with
        | [] -> ()
        | (Compound hd)::tl -> Syntax.print_compound hd ; loop tl
        | (Load f)::_ -> parse_file f; ()
        | _ -> halt ()
    in
        loop l*)
let parse_file filename =
    let fullname = (String.concat ~sep:"" [filename;".pl"]) in
    let inx = In_channel.create fullname in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }; parse lexbuf |> Prolog.assert_f ; Print.color_print (Basic (Red,Bold)) [fullname; " compiled"] ;
 In_channel.close inx 
and parse_cmd cmd = 
    let lexbuf = Lexing.from_string cmd in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = cmd };
  parse lexbuf

(* standard predicates *)
let assertf = function
    [f] -> parse_file f 
    | _ -> assert false 

let halt = fun () -> (print_endline "Halt." ; exit 0)

let exec_std_function (Op (x,y)) = 
    let rec extract_vals acc = function
        | [] -> List.rev acc
        | (Const x)::xs | (Var x)::xs -> extract_vals (x::acc) xs
        | _ -> assert false
    in match x with
    | "print" -> Prolog.print_database () ; true
    | "halt" -> halt ()
    | "assert" -> assertf (extract_vals [] y) ; true
    | _ -> false

let solve cmd = 
    match parse_cmd cmd with
    | ({ head = q ; body = [] })::_ -> (match exec_std_function q with
                                        | true -> ()
                                        | false -> Prolog.solve_prompt q)
    | _ -> ()

let prompt () = 
    try
        while true do
                print_string "?- ";
                let cmd = read_line () in
                    begin 
                        try solve cmd
                            with Match_failure (x,y,z) -> print_endline ("Match_failure, not a valid command " ^ x ^ ":" ^ (string_of_int y)^":"^(string_of_int z)) 
                    end
        done
    with End_of_file -> halt ()

let () =
    prompt ()
(*let () =
  Command.basic ~summary:"Parse Prolog"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(
        empty
        +> flag "-f" (optional file) ~doc:"file Parse the file"
        )
    (fun file () -> match file with
    | Some f -> parse f ; prompt
    | None -> prompt
    )

|> Command.run*)

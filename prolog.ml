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
open Hashtbl
open Print

exception No_pred
exception No_unify of string
exception No_solution of string

type varN = string
type opN = string
type term = 
    | Var of varN
    | Const of varN
    | Op of opN * (term list)

type sub = (varN * term) list

type clause = { head : term ; body : term list }

let prog = Hashtbl.create ~random:false 1

let strip_depth x =
    try let regex = Str.regexp "['__'][0-9]+" in
        let i = (Str.search_forward regex x 0) - 1 in
            String.sub x 0 i
    with Not_found -> x

let rec print_term = function
        [] -> ()
        | x::[] -> (match x with
                    | Const x | Var x -> print_string (strip_depth x)
                    | Op (x,y) -> print_string (x ^ "(") ; print_term y ; print_string ")")
        | x::xs -> (match x with
                    | Const x | Var x -> print_string ((strip_depth x) ^ ", ") ; print_term xs
                    | Op (x,y) -> print_string (x ^ "(") ; print_term y ; print_string ")" ; print_term xs)

let print_database () =
    Hashtbl.iter (fun k { head = h; body = b } -> print_endline k ; print_term [h] ; print_endline ""; print_term b) prog

let notate = function
    Var x -> x ^ "/0"
    | Op (x,y) -> x ^ "/" ^ string_of_int (List.length y)

(* occurs check *)
let rec occurs (x:  varN) = function 
    Var y -> x=y
    | Op (_, l) -> List.exists (occurs x) l
    | Const _ -> false

let rec subst (s : term) (x : varN) = function 
     Var y when x = y -> s
    | Var y -> Var y
    | Const x -> Const x
    | Op (f, u) -> Op (f, List.map (subst s x) u) 

let apply (s: sub) (t: term) : term =
    List.fold_right (fun (x,y) -> subst y x ) s t

let lapply s tl =
    List.map (apply s) tl

let rec unify_one (s: term) (t: term) : sub =
    match (s, t) with
    | s,t when s=t -> []
    | (Var x, t) | (t, Var x)-> (match occurs x t with
                                    | true -> raise (No_unify "circularity")
                                    | false -> [(x, t)])
    | _, Const _ -> raise (No_unify "can't unify constant")
    | (Op (f,sc), Op (g,tc)) when (notate s) = (notate t) -> unify (List.combine sc tc)
    | (Op _, Op _) -> raise (No_unify "circularity")
    | _ -> raise (No_unify "Match error?")
and unify (s : (term * term) list) : sub =
    match s with
    | [] -> []
    | (x,y) :: tl -> let t2 = unify tl in
                    let t1 = unify_one (apply t2 x) (apply t2 y) in
                    t1 @ t2

(* add clauses to database *)
let assert_f (l : clause list) =
    List.iter (fun i -> Hashtbl.add prog (notate i.head) i) l

(* get predicate notation f and return list of database entries *)
let predicate f =
    try List.rev (Hashtbl.find_all prog f) (* Hashtbl.find_all returns most recent entries first *)
    with Not_found -> raise No_pred

(* renames variables to avoid confusion in case of identical variable names *)
let rename_depth n c =
    let rec aux = function
        Var x -> Var (x ^ "__" ^ (string_of_int n))
        | Const x -> Const x
        | Op (x,y) -> Op (x, (List.map aux y))
    in { head = (aux c.head) ; body = (List.map aux c.body) }

let rec print_sub = function
    [] -> print_endline ""
    | (x,y)::xs -> print_string ("Substitution: (Var " ^ x ^ ")=") ; print_term [y]; print_sub xs


let resolution clause (b::gs) q =
    let u = unify [(clause.head,b)] in
    (lapply u (clause.body @ gs), apply u q)

(* (g = goals list, q = query, clausesleft, n = depth level, ch = choices list *)
let rec solve (g : term list) (query : term) (cls : clause list) (n : int) (ch : (term list * term * clause list * int) list) = 
    match g with
    | [] -> (query,ch)
    | x::xs as g -> match cls with
                | [] -> next_choice ch (*raise (No_solution "no clauses left") *)
                | ((a::cs) :clause list) -> try (match resolution (rename_depth n a) g query with
                                            | ([],newq) -> solve [] newq cs (n+1) ((g,query,cs,n)::ch)
                                            | (newg,newq) -> solve newg newq (predicate (notate (List.hd newg))) (n+1) ((g,query,cs,n)::ch)) 
                                            with No_unify _ -> solve g query cs n ch
and next_choice = function (* gets next path in predicate database - backtracking *)
      [] -> raise (No_solution "No choices left")
    | (g, q, cls, n)::tl -> solve g q cls n tl

(* unify result with query, if substitution = [] then print yes, otherwise print the substitution. *)
let rec success (result,choices) query = 
    let rec print_bindings = function
        [] -> ()
        | (x,y)::tl -> print_string (" " ^ x ^ " = ") ; print_term [y] ; print_bindings tl
    in match unify_one result query with
                        | [] -> Print.color_print (Basic (Green,Bold)) ["yes."]
                        | list -> print_bindings list ; try match read_line () with (* search for next choice if user requests with ';\n' *)
                                                            | ";" -> success (next_choice choices) query ; ()
                                                            | _ -> print_string ""
                                                        with (No_solution _) -> print_string ""

let solve_prompt q = try success (solve [q] q (predicate (notate q)) 0 []) q
                    with (No_solution _) -> Print.color_print (Basic (Red,Bold)) ["no."] 

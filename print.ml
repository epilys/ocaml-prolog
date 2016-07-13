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

(* terminal colours *)

type basic_color =
       | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let basic_color_to_int = function
      | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3 | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7

type weight = Regular | Bold

type color =
  | Basic of basic_color * weight (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color cube *)
  | Gray  of int                   (* 24 grayscale levels *)

let color_by_number number text = sprintf "\027[38;5;%dm%s\027[0m" number text;;

let color_to_int = function
    | Basic (basic_color,weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i

let rec color_print color s = match s with
    | [] -> printf "\n"
    | x::xs -> printf "%s" (color_by_number (color_to_int color) x) ; color_print color xs



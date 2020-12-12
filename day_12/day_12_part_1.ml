# load "str.cma";;

exception UnexpectedSituation of string;;
exception UnexpectedInput of string;;

type coordinates = (int * int);;
type angle = int;;
type position = (coordinates * angle);;
type instruction = (char * int);;
type axis =
  | Longitude
  | Latitude
;;

let manhattan_distance_from_origin (pos : position) : int =
  match pos with
  | ((x, y), _) -> (Int.abs x) + (Int.abs y)
;;

let move_in_direction (i : int) (a : axis) (pos : position) : position =
  match pos with
  | ((x, y), angle) ->
    (
      match a with
      | Latitude -> ((x, y+i), angle)
      | Longitude -> ((x+i, y), angle)
    )
;;

let move_forward (i : int) (pos : position) : position =
  match pos with
  | (coord, 0) -> move_in_direction i Longitude pos
  | (coord, 90) -> move_in_direction i Latitude pos
  | (coord, 180) -> move_in_direction (-i) Longitude pos
  | (coord, 270) -> move_in_direction (-i) Latitude pos
  | (_, angle) -> raise (UnexpectedSituation (String.concat "move_forward " [string_of_int angle]))
;;

let change_angle (i : int) (pos : position) : position =
  match (i mod 90) with
  | 0 ->
    (
      match pos with
      | (coord, angle) -> (coord, (angle + i + 360) mod 360)
    )
  | _ -> raise (UnexpectedInput (String.concat "change_angle " [string_of_int i]))
;;

let run_instruction (ins : instruction) (pos : position) : position =
  match ins with
  | ('N', i) -> move_in_direction i Latitude pos
  | ('S', i) -> move_in_direction (-i) Latitude pos
  | ('E', i) -> move_in_direction i Longitude pos
  | ('W', i) -> move_in_direction (-i) Longitude pos
  | ('L', i) -> change_angle i pos
  | ('R', i) -> change_angle (-i) pos
  | ('F', i) -> move_forward i pos
  | (char, i) -> raise (UnexpectedInput (String.concat "run_instruction " [Char.escaped char]))
;;

let interpret_instruction (line : string) : instruction =
  let pattern = Str.regexp "^\\([N|S|E|W|L|R|F]\\)\\([0-9]+\\)$" in
  let char = String.get (Str.replace_first pattern "\\1" line) 0 in
  let int = int_of_string (Str.replace_first pattern "\\2" line) in
  (char, int)
;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s::acc)
    | None -> close_in ic; acc in
  loop []
;;

let destination_from_file (filename : string) : int =
  let list = List.rev (read_lines filename) in
  let instructions = List.map interpret_instruction list in
  let destination_position = List.fold_left (fun pos ins -> run_instruction ins pos) ((0,0), 0) instructions in
  manhattan_distance_from_origin destination_position
;;

Printf.printf "%d\n" (destination_from_file Sys.argv.(1))

# load "str.cma";;

exception UnexpectedSituation of string;;
exception UnexpectedInput of string;;

type coordinates = (int * int);;
type instruction = (char * int);;
type axis =
  | Longitude
  | Latitude
;;

let manhattan_distance_from_origin (coord : coordinates) : int =
  match coord with
  | (x, y) -> (Int.abs x) + (Int.abs y)
;;

let move_in_direction (i : int) (a : axis) (waypoint : coordinates) : coordinates =
  match waypoint with
  | (x, y) ->
    (
      match a with
      | Latitude -> (x, y+i)
      | Longitude -> (x+i, y)
    )
;;

let move_forward (i : int) (waypoint : coordinates) (coord : coordinates) : coordinates =
  match waypoint with
  | (w_x, w_y) ->
    (
      match coord with
      (p_x, p_y) -> ((i*w_x) + p_x, (i*w_y) + p_y)
    )
;;

let change_waypoint_angle (i : int) (waypoint : coordinates) : coordinates =
  match waypoint with
  | (x,y) ->
  (
    match ((i mod 360) + 360) mod 360 with
    | 0 -> waypoint
    | 90 -> (-y, x)
    | 180 -> (-x, -y)
    | 270 -> (y, -x)
    | _ -> raise (UnexpectedInput (String.concat "change_waypoint_angle" [string_of_int i]))
  )
;;

let run_instruction (ins : instruction) ((waypoint : coordinates), (coord : coordinates)) : (coordinates * coordinates) =
  match ins with
  | ('N', i) -> (move_in_direction i Latitude waypoint, coord)
  | ('S', i) -> (move_in_direction (-i) Latitude waypoint, coord)
  | ('E', i) -> (move_in_direction i Longitude waypoint, coord)
  | ('W', i) -> (move_in_direction (-i) Longitude waypoint, coord)
  | ('L', i) -> (change_waypoint_angle i waypoint, coord)
  | ('R', i) -> (change_waypoint_angle (-i) waypoint, coord)
  | ('F', i) -> (waypoint, move_forward i waypoint coord)
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

let destination_from_file (filename : string) (* : int *) =
  let list = List.rev (read_lines filename) in
  let instructions = List.map interpret_instruction list in
  let (_, destination_coordinates) = List.fold_left (fun pos ins -> run_instruction ins pos) ((10,1), (0,0)) instructions in
  manhattan_distance_from_origin destination_coordinates
;;

Printf.printf "%d\n" (destination_from_file Sys.argv.(1))

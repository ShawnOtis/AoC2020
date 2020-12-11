exception UnexpectedSituation;;
exception UnexpectedInput;;

module IntMap = Map.Make(Int);;

type position_map = (char IntMap.t) IntMap.t;;

let seat_status (line_index : int) (row_index : int) (map : position_map) : char =
  IntMap.find row_index (IntMap.find line_index map)
;;

let map_line_size (map : position_map) : int =
  IntMap.cardinal map
;;

let map_row_size (map : position_map) : int =
  IntMap.cardinal (IntMap.find 0 map)
;;

let rec is_seat_occupied_in_vector (line_index : int) (row_index : int) (line_size : int) (row_size : int) ((line_vector : int), (row_vector : int)) (map : position_map) : bool =
  let rec is_seat_occupied_in_vector_h (line_index : int) (row_index : int) : bool =
    match (line_index >= 0) && (line_index < line_size) && (row_index >= 0) && (row_index < row_size) with
    | false -> false
    | true ->
      (
        match IntMap.find row_index (IntMap.find line_index map) with
        | '#' -> true
        | 'L' -> false
        | '.' -> is_seat_occupied_in_vector_h (line_index + line_vector) (row_index + row_vector)
        | _ -> raise UnexpectedInput
      )
  in
  is_seat_occupied_in_vector_h (line_index + line_vector) (row_index + row_vector)
;;

let surrounding_occupied_vectors (line_index : int) (row_index : int) (map : position_map) : int =
  let line_size = map_line_size map in
  let row_size = map_row_size map in
  let vectors = [
    (-1, -1);
    (-1, 0);
    (-1, +1);
    (0, -1);
    (0, +1);
    (+1, -1);
    (+1, 0);
    (+1, +1)] in
  let occupied_vectors = List.filter (fun vector -> is_seat_occupied_in_vector line_index row_index line_size row_size vector map) vectors in
  List.length occupied_vectors
;;

let transform_at_position (line_index : int) (row_index : int) (map : position_map) : char =
  match (seat_status line_index row_index map) with
  | '.' -> '.'
  | '#' -> (match (surrounding_occupied_vectors line_index row_index map) >= 5 with | true -> 'L' | false -> '#')
  | 'L' -> (match (surrounding_occupied_vectors line_index row_index map) > 0 with | true -> 'L' | false -> '#')
  | _ -> raise UnexpectedInput
;;

let occupied_seats_number (map : position_map) : int =
  let occupied_seats = IntMap.map (fun inner_map -> IntMap.filter (fun row_index value -> value = '#') inner_map) map in
  IntMap.fold (fun _ inner_map acc -> acc + (IntMap.cardinal inner_map)) occupied_seats 0
;;

let apply_round (map : position_map) : position_map =
  IntMap.mapi (fun line_index inner_map -> IntMap.mapi (fun row_index status -> transform_at_position line_index row_index map) inner_map) map
;;

let rec find_fixed_point (map : position_map) : position_map =
  let next_round = apply_round map in
  match (IntMap.bindings next_round )= (IntMap.bindings map) with
  | true -> map
  | false -> find_fixed_point next_round
;;

let interpret_line (line : string) : char IntMap.t =
  let rec interpret_line_h (line : string) (index : int) (map : char IntMap.t) : char IntMap.t =
    match Int.compare index (String.length line) with
    | 0 -> map
    | -1 -> interpret_line_h line (index + 1) (IntMap.add index (String.get line index) map)
    | _ -> raise UnexpectedSituation
  in
  interpret_line_h line 0 IntMap.empty
;;

(* Note that the map will be indexed first by line index, and then by row index (vertical then horizontal) *)
let interpret_lines (lines : string list) : position_map =
  let rec interpret_lines_h (lines : string list) (index : int) (map : position_map) : position_map =
    match lines with
    | [] -> map
    | h::tl -> interpret_lines_h tl (index + 1) (IntMap.add index (interpret_line h) map)
  in
  interpret_lines_h lines 0 IntMap.empty
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

let available_seats_from_file (filename : string) (* : int *) =
  let list = read_lines filename in
  let position_map = interpret_lines (List.rev list) in
  let final_map = find_fixed_point position_map in
  occupied_seats_number final_map
;;

Printf.printf "%d\n" (available_seats_from_file Sys.argv.(1))

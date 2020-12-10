exception UnexpectedSituation;;
exception NoInvalidValueFound;;
module IntMap = Map.Make(Int);;

let rec is_sum_of_two_values (value : int) (sorted_list : int list) : bool =
  let rec is_sum_of_two_values_h (value : int) (current : int) (sorted_list : int list) (cont : unit -> bool) : bool =
    match sorted_list with
    | [] -> cont ()
    | h::tl -> (
        match Int.compare value (current + h) with
        | 0 -> true
        | +1 -> is_sum_of_two_values_h value current tl cont
        | -1 -> cont ()
        | _ -> raise UnexpectedSituation
      )
  in
  match sorted_list with
  | [] -> false
  | h::tl ->
    (
      match 2*h > value with
      | true -> false
      | false ->  is_sum_of_two_values_h value h tl (fun () -> is_sum_of_two_values value tl)
    )
;;

let is_valid_index (index : int) (preamble_size : int) (map : int IntMap.t) : bool =
  let preamble_map = (IntMap.filter (fun key _ -> key < index && key >= (index - preamble_size)) map) in
  let preamble_list = (List.map (fun x -> match x with | (key, value) -> value) (IntMap.bindings preamble_map)) in
  let sorted_preamble = List.sort (Int.compare) preamble_list in
  let value_at_index = IntMap.find index map in
  is_sum_of_two_values value_at_index sorted_preamble
;;

let first_invalid_value (preamble_size : int) (map : int IntMap.t) : int =
  let potential_indices = List.init ((IntMap.cardinal map) - preamble_size) (fun x -> preamble_size + x) in
  let rec first_invalid_value_h (indices : int list) : int =
    match indices with
    | [] -> raise NoInvalidValueFound
    | index::tl ->
      (
        match is_valid_index index preamble_size map with
        | true -> first_invalid_value_h tl
        | false -> IntMap.find index map
      )
  in
  first_invalid_value_h potential_indices
;;

let read_lines name : string IntMap.t =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc index = match try_read () with
    | Some s -> loop (IntMap.add index s acc) (index + 1)
    | None -> close_in ic; acc in
  loop IntMap.empty 0
;;

let invalid_value_from_file (filename : string) (preamble_size : int) : int =
  let map = read_lines filename in
  let int_map = IntMap.map int_of_string map in
  first_invalid_value preamble_size int_map
;;

Printf.printf "%d\n" (invalid_value_from_file Sys.argv.(1) (int_of_string Sys.argv.(2)))

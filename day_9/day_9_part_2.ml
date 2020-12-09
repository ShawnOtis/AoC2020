exception UnexpectedSituation;;
exception NoInvalidValueFound;;
exception NoContiguousSetFound;;
module IntMap = Map.Make(Int);;

let int_map_to_value_list (map : int IntMap.t) : int list =
  List.map (fun x -> match x with | (key, value) -> value) (IntMap.bindings map)
;;

let rec find_contiguous_set (value :int) (list : int list) : int list =
  let rec find_contiguous_set_h (value : int) (list : int list) (potential_set : int list) (sum : int) (cont : unit -> int list) : int list =
    match (Int.compare value sum) with
    | -1 -> cont ()
    | 0 ->
      (
        match (List.length potential_set) = 1 with
        | true -> cont ()
        | false -> potential_set
      )
    | +1 ->
      (
        match list with
        | [] -> cont ()
        | h::tl -> find_contiguous_set_h value tl (h::potential_set) (sum + h) cont
      )
    | _ -> raise UnexpectedSituation
  in
  match list with
  | [] -> raise NoContiguousSetFound
  | h::tl -> find_contiguous_set_h value tl [h] h (fun () -> find_contiguous_set value tl)
;;

let encryption_weakness (value) (map : int IntMap.t) : int =
  let contiguous_set = find_contiguous_set value (int_map_to_value_list map) in
  match contiguous_set with
  | [] -> raise NoContiguousSetFound
  | h::tl ->
    (
      let minimum = List.fold_left (fun acc value -> match value < acc with |true -> value |false -> acc) h tl in
      let maximum = List.fold_left (fun acc value -> match value > acc with |true -> value |false -> acc) h tl in
      minimum + maximum
    )
;;

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
  let preamble_list = int_map_to_value_list preamble_map in
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

let encryption_weakness_from_file (filename : string) (preamble_size : int) : int =
  let map = read_lines filename in
  let int_map = IntMap.map int_of_string map in
  let invalid_value = first_invalid_value preamble_size int_map in
  encryption_weakness invalid_value int_map
;;

Printf.printf "%d\n" (encryption_weakness_from_file Sys.argv.(1) (int_of_string Sys.argv.(2)))

exception UnexpectedError;;
exception NoCombinationFound;;

let find_combinations (limit : int) (list : (int * int) list) : int =
  let rec find_combinations_h (limit : int) (list : (int * int) list) (sum : int) (acc : int) : int =
    match list with
    | [] -> acc
    | (value, combinations)::tl ->
      (
        match Int.compare (sum + value) limit with
        | 0 | -1 -> find_combinations_h limit tl (sum + value) (acc + combinations)
        | +1 -> acc
        | _ -> raise UnexpectedError
      )
  in
  match list with
  | [] -> 1
  | _ -> find_combinations_h limit list 0 0
;;

let calculate_combinations (list : int list) (limit : int) : int =
  let rec calculate_combinations_h (list : int list) (limit : int) (acc : (int * int) list) : (int * int) list =
  match list with
  | [] -> acc
  | h::tl -> calculate_combinations_h tl limit ((h, find_combinations limit acc)::acc)
  in
  let combinations_list = calculate_combinations_h list limit [] in
  match combinations_list with
  | (steps, value)::tl -> value
  | _ -> raise NoCombinationFound
;;

let rec calculate_differences (ordered_list : int list) (limit : int) : int list =
  let rec calculate_differences_h (list : int list) (acc : int list) : int list =
  match list with
  | [] -> acc
  | h::[] -> limit::acc
  | h1::h2::tl -> calculate_differences_h (h2::tl) ((h2-h1)::acc)
  in
  calculate_differences_h (0::ordered_list) []
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

(* Since we're including the plug's difference with value of 0, and the device's difference of value equal to the limit, we need to reverse the list to ensure we don't count extras for the plug, and we're fine with the device, since it's equal to the difference *)
let joltage_value_from_file (filename : string) : int =
  let list = read_lines filename in
  let joltages = List.map int_of_string list in
  let ordered_joltages = List.sort Int.compare joltages in
  let limit = 3 in
  let differences = calculate_differences ordered_joltages limit in
  calculate_combinations (List.rev differences) limit
;;

Printf.printf "%d\n" (joltage_value_from_file Sys.argv.(1))

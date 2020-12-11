let value_differences (value : int) (list : int list) : int =
  List.length (List.filter (fun x -> x = value) list)
;;

let rec calculate_product (list : int list) (values : int list) : int =
  List.fold_left (fun acc value -> acc * (value_differences value list)) 1 values
;;

let rec calculate_differences (ordered_list : int list) : int list =
  let rec calculate_differences_h (list : int list) (acc : int list) : int list =
  match list with
  | [] -> acc
  | h::[] -> 3::acc
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

let joltage_value_from_file (filename : string) : int =
  let list = read_lines filename in
  let joltages = List.map int_of_string list in
  let ordered_joltages = List.sort Int.compare joltages in
  let differences = calculate_differences ordered_joltages in
  calculate_product differences [1;3]
;;

Printf.printf "%d\n" (joltage_value_from_file Sys.argv.(1))

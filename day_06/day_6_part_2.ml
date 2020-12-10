type answers = char list;;
type group_answers = answers list

let yes_answers_for_group (group_answers : group_answers) : answers =
  match group_answers with
  | [] -> []
  | h::tl -> List.filter (fun base_char -> List.for_all (List.exists (fun char -> char = base_char)) tl) h
;;

let string_to_ordered_char_list (s : string) : char list =
  List.sort (Char.compare) (List.init (String.length s) (fun i -> String.get s i))
;;

let convert_to_group_answers (list : string list) : group_answers list =
  let rec split_by_group_h (list : string list) (acc : string list list) : string list list =
    match list with
    | [] -> acc
    | ""::tl -> split_by_group_h tl ([]::acc)
    | s::tl ->
      (
        match acc with
        | [] -> split_by_group_h tl [s::[]]
        | acc_h::acc_tl -> split_by_group_h tl ((s::acc_h)::acc_tl)
      )
  in
  let group_answer_strings = List.filter (fun x -> x != []) (split_by_group_h list []) in
  List.map (List.map string_to_ordered_char_list) group_answer_strings
;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; acc in
  loop []
;;

let highest_ticket_from_file (filename : string) : int =
  let list = read_lines filename in
  let group_answers = convert_to_group_answers list in
  let answers_in_group = List.map yes_answers_for_group group_answers in
  List.fold_left ( + ) 0 (List.map List.length answers_in_group)
;;

Printf.printf "%d\n" (highest_ticket_from_file Sys.argv.(1))

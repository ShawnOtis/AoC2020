#load "str.cma";;
exception NoAvailableSeat;;

type ticket = int * int;;

let bool_to_bit (b : bool) : int =
  match b with
  | true -> 1
  | false -> 0
;;

let convert_string_to_binary (s : string) (bit_one_character : char) : int list =
  List.init (String.length s) (fun x -> bool_to_bit ((String.get s x) = bit_one_character))
;;

let ticket_section_value (l : int list) =
  List.fold_left (fun acc bit -> (acc * 2) + bit) 0 l
;;

let interpret_ticket (ticket_string : string) : ticket =
  let pattern = Str.regexp "^\\(\\(F\\|B\\)*\\)\\(\\(R\\|L\\)*\\)$" in
  let row_string = Str.replace_first pattern "\\1" ticket_string in
  let column_string = Str.replace_first pattern "\\3" ticket_string in
  let row_list = convert_string_to_binary row_string 'B' in
  let column_list = convert_string_to_binary column_string 'R' in
  (ticket_section_value row_list, ticket_section_value column_list)
;;

let ticket_value (ticket : ticket) : int =
  match ticket with
  | (r, c) -> 8*r + c
;;

let rec find_seat (ticket_values : int list) : int =
  match ticket_values with
  | s1::s2::tl ->
    (
      match (s1 + 2) = s2 with
      | true -> s1 + 1
      | false -> find_seat (s2::tl)
    )
  | _ -> raise NoAvailableSeat
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

let highest_ticket_from_file (filename : string) (* : int *) =
  let list = read_lines filename in
  let tickets = List.map interpret_ticket list in
  let ticket_values = List.sort Int.compare (List.map ticket_value tickets) in
  find_seat ticket_values
;;

Printf.printf "%d\n" (highest_ticket_from_file Sys.argv.(1))

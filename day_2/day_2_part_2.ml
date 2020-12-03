type policy = int * int * char;;
type password = string;;
type password_with_policy = policy * password;;

let convert_substring_to_char_list (s : string) (start : int) (length: int) : char list =
  List.init length (fun x -> String.get s (start + x))
;;

let valid_password (tup : password_with_policy) : bool =
  match tup with
  | ((pos_1, pos_2, character), word) ->
    let list = [String.get word (pos_1 - 1); String.get word (pos_2 - 1)] in
    List.length ( List.filter (fun x -> x = character) list ) = 1
;;

let valid_passwords (list : password_with_policy list) : int =
  List.length (
    List.filter valid_password list
  )
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

let interpret_line (line : string) : password_with_policy =
  let min_break = String.index line '-' in
  let max_break = String.index line ' ' in
  let character_break = String.index line ':' in
  let min = int_of_string (String.sub line 0 (min_break)) in
  let max = int_of_string (String.sub line (min_break + 1) (max_break - min_break - 1)) in
  let char = String.get line (max_break + 1) in
  let word = String.sub line (character_break + 2) ((String.length line) - character_break - 2) in
  ((min, max, char), word)
;;

let valid_passwords_from_file (filename : string) : int =
  let list = List.map interpret_line (read_lines filename) in
  valid_passwords list
;;

let result = valid_passwords_from_file Sys.argv.(1)
;;

Printf.printf "%d\n" result

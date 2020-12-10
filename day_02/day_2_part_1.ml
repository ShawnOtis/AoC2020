type policy = int * int * char;;
type password = char list;;
type password_with_policy = policy * password;;

let valid_password (tup : password_with_policy) : bool =
  match tup with
  | ((min, max, character), word) ->
    let appearences = List.length (List.filter (fun x -> x = character) word) in
    min <= appearences && appearences <= max
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

let convert_substring_to_char_list (s : string) (start : int) (length: int) : char list =
  List.init length (fun x -> String.get s (start + x))
;;

let interpret_line (line : string) : password_with_policy =
  let min_break = String.index line '-' in
  let max_break = String.index line ' ' in
  let character_break = String.index line ':' in
  let min = int_of_string (String.sub line 0 (min_break)) in
  let max = int_of_string (String.sub line (min_break + 1) (max_break - min_break - 1)) in
  let char = String.get line (max_break + 1) in
  let word = convert_substring_to_char_list line (character_break + 2) ((String.length line) - character_break - 2) in
  ((min, max, char), word)
;;

let valid_passwords_from_file (filename : string) : int =
  let list = List.map interpret_line (read_lines filename) in
  valid_passwords list
;;

let result = valid_passwords_from_file Sys.argv.(1)
;;

Printf.printf "%d\n" result

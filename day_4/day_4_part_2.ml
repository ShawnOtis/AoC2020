#load "str.cma";;
exception UnexpectedInput;;

type passport = string option list

let passport_keys = ["byr"; "ecl"; "eyr"; "hcl"; "hgt"; "iyr"; "pid"];;

let byr_condition (value : string) : bool =
  let pattern = Str.regexp "^[0-9][0-9][0-9][0-9]$" in
  let regex_condition = Str.string_match pattern value 0 in
  regex_condition && (int_of_string value) >= 1920 && (int_of_string value) <= 2002
;;

let iyr_condition (value : string) : bool =
  let pattern = Str.regexp "^[0-9][0-9][0-9][0-9]$" in
  let regex_condition = Str.string_match pattern value 0 in
  regex_condition && (int_of_string value) >= 2010 && (int_of_string value) <= 2020
;;

let eyr_condition (value : string) : bool =
  let pattern = Str.regexp "^[0-9][0-9][0-9][0-9]$" in
  let regex_condition = Str.string_match pattern value 0 in
  regex_condition && (int_of_string value) >= 2020 && (int_of_string value) <= 2030
;;

let hgt_condition (value : string) : bool =
  let pattern = Str.regexp "^\\([0-9]+\\)\\(cm\\|in\\)$" in
  let regex_condition = Str.string_match pattern value 0 in
  regex_condition &&
  (
    let units = Str.replace_first pattern "\\2" value in
    let digits = Str.replace_first pattern "\\1" value in
    match units with
    | "cm" -> (int_of_string digits >= 150) && (int_of_string digits <= 193)
    | "in" -> (int_of_string digits >= 59) && (int_of_string digits <= 76)
    | _ -> raise UnexpectedInput
  )
;;

let hcl_condition (value : string) : bool =
  let pattern = Str.regexp "^#\\([0-9]\\|[a-f]\\)\\([0-9]\\|[a-f]\\)\\([0-9]\\|[a-f]\\)\\([0-9]\\|[a-f]\\)\\([0-9]\\|[a-f]\\)\\([0-9]\\|[a-f]\\)$" in
  Str.string_match pattern value 0
;;

let ecl_condition (value : string) : bool =
  let pattern = Str.regexp "^\\(amb\\|blu\\|brn\\|gry\\|grn\\|hzl\\|oth\\)$" in
  Str.string_match pattern value 0
;;

let pid_condition (value : string) : bool =
  let pattern = Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" in
  Str.string_match pattern value 0
;;

let format_passport (passport_string : string list) : passport=
  let ordered_passport_information = List.sort String.compare passport_string in
  let passport_info = List.map (String.split_on_char ':') ordered_passport_information in

  let rec format_passport_h (passport_string : string list list) (keys_remaining : string list) (acc : string option list) : passport =
    match keys_remaining with
    | [] -> acc
    | key::keys ->
      (
        match passport_string with
        | [] -> format_passport_h [] keys (None::acc)
        | [k;v]::tl ->
          (
            match String.compare key k with
            | 0 -> format_passport_h tl keys ((Some v)::acc)
            | -1 -> format_passport_h passport_string keys (None::acc)
            | 1 -> format_passport_h tl keys_remaining acc
            | _ -> raise UnexpectedInput
          )
        | _ -> raise UnexpectedInput
      )
  in
  List.rev (format_passport_h passport_info (List.sort String.compare passport_keys) [])
;;

let valid_passport (passport : passport) : bool =
  match passport with
  | [Some byr; Some ecl; Some eyr; Some hcl; Some hgt; Some iyr; Some pid] ->
    (
      byr_condition byr && ecl_condition ecl && eyr_condition eyr &&
      hcl_condition hcl && hgt_condition hgt && iyr_condition iyr &&
      pid_condition pid
    )
  | _ -> false
;;

let aggregate_lists (accumulator : string list list) (elem : string) : string list list =
  match elem with
  | "" ->
    (
      match accumulator with
      | [] -> []
      | l -> []::l
    )
  | s -> match accumulator with
    | [] -> (s::[])::[]
    | h::tl -> (s::h)::tl
;;

let split_lines (list : string list) : string list list =
  let flat_list =
    List.flatten (List.map (String.split_on_char ' ') list)
  in
  List.fold_left aggregate_lists [] flat_list
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

let valid_passports_from_file (filename : string) : int =
  let list = List.rev(read_lines filename) in
  let passport_strings = split_lines list in
  let formatted_passports = List.map format_passport passport_strings in
  List.length (List.filter valid_passport formatted_passports)
;;

Printf.printf "%d\n" (valid_passports_from_file Sys.argv.(1))

# load "str.cma";;
exception UnexpectedCommand;;
exception NoBugFound;;

module InsMap = Map.Make(Int);;

type instruction = string * int;;
type instruction_map = instruction InsMap.t;;

let find_bug (map : instruction_map) : int =
  let rec find_bug_post_mod_h (map : instruction_map) (index : int) (visited : unit InsMap.t) (acc : int) (cont : unit -> int) : int =
    match index = (InsMap.cardinal map) with
    | true -> acc
    | false ->
      (
        match InsMap.mem index visited with
        | true -> cont ()
        | false ->
          (
            match InsMap.find index map with
            | ("acc", offset) -> find_bug_post_mod_h map (index + 1) (InsMap.add index () visited) (acc + offset) cont
            | ("jmp", offset) -> find_bug_post_mod_h map (index + offset) (InsMap.add index () visited) (acc) cont
            | ("nop", offset) -> find_bug_post_mod_h map (index + 1) (InsMap.add index () visited) (acc) cont
            | _ -> raise UnexpectedCommand
          )
      )
  in
  let rec find_bug_before_mod_h (map : instruction_map) (index : int) (visited : unit InsMap.t) (acc : int) : int =
    match InsMap.mem index visited with
    | true -> raise NoBugFound
    | false ->
      (
        match InsMap.find index map with
        | ("acc", offset) -> find_bug_before_mod_h map (index + 1) (InsMap.add index () visited) (acc + offset)
        | ("jmp", offset) -> find_bug_post_mod_h map (index + 1) (InsMap.add index () visited) (acc) (fun () -> find_bug_before_mod_h map (index + offset) (InsMap.add index () visited) (acc))
        | ("nop", offset) -> find_bug_post_mod_h map (index + offset) (InsMap.add index () visited) (acc) (fun () -> find_bug_before_mod_h map (index + 1) (InsMap.add index () visited) (acc))
        | _ -> raise UnexpectedCommand
      )
  in
  find_bug_before_mod_h map 0 InsMap.empty 0
;;

let interpret_instruction (s : string) : instruction =
  let pattern = Str.regexp "^\\(.+\\) \\(-[0-9]+\\|\\+[0-9]+\\)$" in
  let command = Str.replace_first pattern "\\1" s in
  let offset = int_of_string (Str.replace_first pattern "\\2" s) in
  (command, offset)
;;

let read_lines name : string InsMap.t =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc index = match try_read () with
    | Some s -> loop (InsMap.add index s acc) (index + 1)
    | None -> close_in ic; acc in
  loop InsMap.empty 0
;;

let instructions_value_from_file (filename : string) : int =
  let map = read_lines filename in
  let ins_map = InsMap.map interpret_instruction map in
  find_bug ins_map
;;

Printf.printf "%d\n" (instructions_value_from_file Sys.argv.(1))

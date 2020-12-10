# load "str.cma";;
exception UnexpectedCommand;;

module InsMap = Map.Make(Int);;

type instruction = string * int;;
type instruction_map = instruction InsMap.t;;

let calculate_until_loop (map : instruction_map) : int =
  let rec calculate_until_loop_h (map : instruction_map) (index : int) (visited : unit InsMap.t) (acc : int) : int =
    match InsMap.mem index visited with
    | true -> acc
    | false ->
      (
        match InsMap.find index map with
        | ("acc", offset) -> calculate_until_loop_h map (index + 1) (InsMap.add index () visited) (acc + offset)
        | ("jmp", offset) -> calculate_until_loop_h map (index + offset) (InsMap.add index () visited) (acc)
        | ("nop", offset) -> calculate_until_loop_h map (index + 1) (InsMap.add index () visited) (acc)
        | _ -> raise UnexpectedCommand
      )
  in
  calculate_until_loop_h map 0 InsMap.empty 0
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
  calculate_until_loop ins_map
;;

Printf.printf "%d\n" (instructions_value_from_file Sys.argv.(1))

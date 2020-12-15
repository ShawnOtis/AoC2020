exception UnexpectedSituation;;
exception UnexpectedInput;;
exception InputNotCoprimes;;

(* We know that, for every valid buses b_i and b_j meant to leave, respectively,
 * i and j minutes after the timestamp, where m_i and m_j are the durations of a
 * loop for b_i and b_j respectively, and k_i and k_j the number of loops for
 * b_i and b_j respectively:
 *
 *  i - j = m_i*k_i - m_j * k_j
 *
 *  Thus, we have that for all i, the number of rounds  must respect:
 *
 *  m_i * k_i = i - j + m_j * k_j  => i - j + m_j * k_j = 0 mod m_i
 *
 *
 * Assuming that all the loop times are coprime, then the should be a unique value of m_j mod m_i (To confirm).
 *
 * Moreover, once we have these unique values of m_j, we get equations:
 *
 * m_j = 0 mod m_i1
 * m_j = 0 mod m_i2
 * ...
 *
 * Using coprimality, by the Chinese Remainder theorem, there should be a unique value such that (To confirm)
 * m_j = 0 mod m_i1 * m_i2 * ...
*)

(* Using Chinese remainder theorem sieving approach per wikipedia *)
let find_minimum_timestamp_from_mods ((first_bus_index, first_bus_loop) : (int * int)) (other_buses_mod : (int * int) list) : int =
  let rec find_minimum_timestamp_from_mods_h (acc : int * int) (remaining_buses : (int * int) list) : int * int =
    match remaining_buses with
    | [] -> acc
    | (m_i, mod_val_i)::tl ->
      (
        match acc with
        | (m_j, mod_val_j) ->
          (
            match (mod_val_j mod m_i) = mod_val_i with
            | true -> find_minimum_timestamp_from_mods_h (m_i * m_j, mod_val_j) tl
            | false -> find_minimum_timestamp_from_mods_h (m_j, mod_val_j + m_j) remaining_buses
          )
      )
  in
  let list = List.sort (fun (m_i, _) (m_j, _) -> Int.compare m_j m_i) other_buses_mod in
  match list with
  | h::tl -> let (_, modulo_value) = find_minimum_timestamp_from_mods_h h tl in first_bus_loop * modulo_value
  | _ -> raise UnexpectedInput
;;

let find_mod_value (first_bus : (int * int)) (other_bus : (int * int)) : int =
  match first_bus with
  | (j, m_j) ->
    (
      match other_bus with
      | (i, m_i) ->
        (
          let l = List.filter (fun x -> (i - j + (m_j * x)) mod m_i = 0) (List.init m_i (fun x -> x)) in
          match l with
          | h::[] -> h
          | _ -> raise InputNotCoprimes
        )
    )
;;

let calculate_minimum_timestamp (buses : (int * int) list) : int =
  let calculate_minimum_timestamp_h (first_bus : (int * int)) (other_buses : (int * int) list) : int =
    let other_buses_mods = List.map (fun (x, y) -> (y, find_mod_value first_bus (x,y))) other_buses in
    find_minimum_timestamp_from_mods first_bus other_buses_mods
  in
  match buses with
  | h::tl -> calculate_minimum_timestamp_h h tl
  | [] -> raise UnexpectedInput
;;

let number_list (list : 'a list) : (int * 'a) list =
  let rec number_list_h (list : 'a list) (iter : int) (acc : (int * 'a) list) =
    match list with
    | [] -> acc
    | h::tl -> number_list_h tl (iter + 1) ((iter, h)::acc)
  in
  List.rev (number_list_h list 0 [])
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

let bus_from_file (filename : string) : int =
  let list = List.rev (read_lines filename) in
  let (min_string, buses_string) =
    (match list with
     | h1::h2::[] -> (h1, h2)
     | _ -> raise UnexpectedInput
    )
  in
  let bus_list = String.split_on_char ',' buses_string in
  let filtered_buses = List.filter (fun (_, y) -> y <> "x") (number_list bus_list) in
  calculate_minimum_timestamp (List.map (fun (x, y) -> (x, int_of_string y)) filtered_buses)
;;

Printf.printf "%d\n" (bus_from_file Sys.argv.(1))

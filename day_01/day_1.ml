let find_entries (sum : int) (l : int list) (number_of_entries : int) : int option =
  let rec find_entries_sorted (sum : int) (value_candidates : int list) (remaining_values : int list) (entries_left : int) (continuation : unit -> int option) : int option =
    match (Int.compare (List.fold_left (+) 0 value_candidates) sum) with
    | -1 ->
      (
        match (Int.compare entries_left 0) with
        | 1 ->
          (
            match remaining_values with
            | h::tl -> find_entries_sorted sum (h::value_candidates) tl (entries_left - 1) (fun () -> find_entries_sorted sum value_candidates tl entries_left continuation )
            | [] -> continuation ()
          )
        | _ -> continuation ()
      )
    | 0 ->
      (
        match entries_left with
        | 0 -> Some (List.fold_left ( * ) 1 value_candidates)
        | _ -> continuation ()
      )
    | _ -> continuation ()
  in
  find_entries_sorted sum [] (List.sort Int.compare l) number_of_entries (fun () -> None)
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

let find_entries_from_file sum number_of_entries : int option =
  let list = List.map int_of_string (read_lines "day_1_file") in
  find_entries sum list number_of_entries
;;

let result = find_entries_from_file (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2))
;;

match result with
| None -> Printf.printf("No result\n")
| Some value -> Printf.printf "%d\n" value

exception UnexpectedSituation;;
exception UnexpectedInput;;

let earliest_departure_diff (bus_duration : int) (minimum : int) : (int * int) =
  let lower_bound = minimum/bus_duration in
  match lower_bound >= minimum with
  | true -> (bus_duration, ((lower_bound * bus_duration) - minimum))
  | false -> (bus_duration, (((lower_bound + 1) * bus_duration) - minimum))
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

let bus_from_file (filename : string) (* : int *) =
  let list = List.rev (read_lines filename) in
  let (min_string, buses_string) =
    (match list with
     | h1::h2::[] -> (h1, h2)
     | _ -> raise UnexpectedInput
    )
  in
  let minimum = int_of_string min_string in
  let bus_list = String.split_on_char ',' buses_string in
  let valid_buses = List.map int_of_string (List.filter (fun x -> x <> "x") bus_list) in
  let bus_departure_diff = List.map (fun x -> earliest_departure_diff x minimum) valid_buses in
  let (bus_duration, minimum_departure_diff) = List.hd (List.sort (fun (x1, y1) (x2,y2) -> Int.compare y1 y2) bus_departure_diff) in
  bus_duration * minimum_departure_diff
;;

Printf.printf "%d\n" (bus_from_file Sys.argv.(1))

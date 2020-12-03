(* Using a string is not the most functional of solutions, but it was more "natural" *)
type biome_line = string ;;
type biome = biome_line list;;
let tree = '#';;
let open_square = '.';;

(* Opted to take a continuation passing style to have some fun. It would probably make more sense to implement this with an accumulator, and then reverse the list. *)
let filteri (cond : int -> 'a -> bool) (l : 'a list) : 'a list =
  let rec filteri_h (cond : int -> 'a -> bool) (l : 'a list) (pos : int) (cont : 'a list -> 'a list) : 'a list =
    match l with
    | [] -> cont []
    | h::tl ->
      (
        match cond pos h with
        | true -> filteri_h cond tl (pos + 1) (fun l -> cont (h::l))
        | false -> filteri_h cond tl (pos + 1) cont
      )
  in
  filteri_h cond l 0 (fun l -> l)
;;

let tree_collision (line : biome_line) (position : int) : bool =
  (String.get line (position mod (String.length line))) = tree
;;

let trees_in_path (list : biome) (horizontal_shift : int) (vertical_shift : int) : int =
  let relevant_lines = filteri (fun pos _ -> (pos mod vertical_shift) = 0) list in
  let lines_on_trees = filteri (fun i x -> tree_collision x (i*horizontal_shift)) relevant_lines in
  List.length lines_on_trees
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

let trees_in_path_from_file (filename : string) : int =
  let list = List.rev(read_lines filename) in
  let trees_encountered_list = List.map (fun (h,v) -> trees_in_path list h v) [(1,1); (3,1); (5,1); (7,1);(1,2)] in
  List.fold_left ( * ) 1 trees_encountered_list
;;

let result = trees_in_path_from_file Sys.argv.(1)
;;

Printf.printf "%d\n" result

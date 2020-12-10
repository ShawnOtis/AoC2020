# load "str.cma";;

(* Decided to explore Map, which has definitely increased complexity, but which was still interesting *)
module BagMap = Map.Make(String);;

type bag_color = string;;
type bag_content = (int * bag_color);;
type bag = (bag_color * bag_content list);;

type bag_content_map = (int BagMap.t);;
type bag_map = (bag_content_map BagMap.t);;

let number_of_bags (color : bag_color) (map : bag_map) : int =
  let rec number_of_bags_h color map =
    let bag_content_map = BagMap.find color map in
    match BagMap.is_empty bag_content_map with
    | true -> 1
    | false -> 1 + (BagMap.fold (fun key num acc -> acc + num*(number_of_bags_h key map) ) bag_content_map 0)
  in (number_of_bags_h color map) - 1 (* Unfortunately, I am too tired to come up with a better solution that is tail recursive. I may revisit later to refactor it *)
;;

let create_bag_content_map (contents : bag_content list) : bag_content_map =
  List.fold_left (fun map content -> match content with | (number, color) -> BagMap.add color number map) BagMap.empty contents
;;

let create_bag_map (l : bag list) : bag_map =
  let rec create_bag_map_h (l : bag list) (map : (bag_content_map) BagMap.t) : (bag_content_map) BagMap.t =
    match l with
    | [] -> map
    | (color, contents)::tl -> create_bag_map_h tl (BagMap.add color (create_bag_content_map contents) map)
  in
  create_bag_map_h l BagMap.empty
;;

let empty_bag_filter (s : string) : bool =
  let pattern = Str.regexp "no other bag" in
  not (Str.string_match pattern s 0)
;;

let interpret_content_bag (s : string) : bag_content =
  let pattern = Str.regexp "^ *\\([0-9]+\\) \\(.*\\)\\ bag\\(.*\\)$" in
  let digit = int_of_string (Str.replace_first pattern "\\1" s) in
  let bag_color = Str.replace_first pattern "\\2" s in
  (digit, bag_color)
;;

let interpret_line (s : string) : bag =
  let pattern = Str.regexp "^\\(.*\\)\\ bags contain \\(.*\\)$" in
  let bag_color = Str.replace_first pattern "\\1" s in
  let contents_string = Str.replace_first pattern "\\2" s in
  let contents = String.split_on_char ',' contents_string in
  let bag_contents = List.map interpret_content_bag (List.filter empty_bag_filter contents) in
  (bag_color, bag_contents)
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

let bags_from_file (filename : string) : int =
  let list = read_lines filename in
  let bag_list = List.map interpret_line list in
  let bag_map = create_bag_map bag_list in
  (number_of_bags "shiny gold" bag_map)
;;

Printf.printf "%d\n" (bags_from_file Sys.argv.(1))

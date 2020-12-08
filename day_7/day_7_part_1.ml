# load "str.cma";;

(* Decided to explore Map, which has definitely increased complexity, but which was still interesting *)
module BagMap = Map.Make(String);;

type bag_color = string;;
type bag_content = (int * bag_color);;
type bag = (bag_color * bag_content list);;

type bag_content_map = (int BagMap.t);;
type bag_map = (bag_content_map BagMap.t);;

let merge_maps (map1 : unit BagMap.t) (map2 : unit BagMap.t) =
  BagMap.fold (fun key value acc' -> BagMap.add key () acc') map1 map2
;;

let find_bags_containing_color (color : bag_color) (map : bag_map) : unit BagMap.t =
  let rec find_bags_containing_color_h (colors_remaining : unit BagMap.t) (map : bag_map) (acc : unit BagMap.t) =
    match BagMap.bindings colors_remaining with
    | [] -> acc
    | (color, _)::tl ->
      (
        let new_candidate_bags = BagMap.filter (fun key bag_content_map -> BagMap.mem color bag_content_map) map in
        let new_candidate_colors = BagMap.map (fun _ -> ()) new_candidate_bags in
        let filtered_new_candidatee_colors = BagMap.filter (fun key _ -> not (BagMap.mem key colors_remaining)) new_candidate_colors in
        find_bags_containing_color_h (BagMap.remove color (merge_maps filtered_new_candidatee_colors colors_remaining)) map (merge_maps filtered_new_candidatee_colors acc)
      )
  in
  find_bags_containing_color_h (BagMap.add color () BagMap.empty) map (BagMap.add color () BagMap.empty)
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
  let matching_bags = BagMap.remove "shiny gold" (find_bags_containing_color "shiny gold" bag_map) in
  BagMap.cardinal matching_bags
;;

Printf.printf "%d\n" (bags_from_file Sys.argv.(1))


let default_upper_threshhold = 100.0
let default_lower_threshhold = 20.0

(* Default to an unmatchable regexp to include all lines by default.
 * Note: This is unmatchable because there can't be word boundaries within a word! *)
let default_exclude_pattern = Str.regexp "x\by"

(* A line is a record of:
 * - line: the line itself,
 * - lineno: the number the line is on, starting at 1,
 * - matching_on: A vector of start,end pairs of identical regions in the line. *)
type line = { line: string; lineno: int; matching_on: (int * int) Vector.t }

(* A sim is a pair of lines and their percentage of similarity (Between 0.0 and 100.0) *)
type sim = { l1: line; l2: line; sim: float; }

(* Convert a file to a stream to more easily iterate through it *)
let file_stream chan =
  Stream.from (fun _ -> try Some (input_line chan) with End_of_file -> None)


(* Similarity is inversely proportional to Levenshtein distance / average string length *)
let similarity l1 l2 =
  let dist = String_distance.distance l1 l2 in
  let len = max (String.length l1) (String.length l2) in
  if len = 0 then 0. else
    float_of_int (abs (len - dist) * 100) /. float_of_int len


(* Create a sim for the given line pair *)
let find_similarities line1 line2 lineno1 lineno2 = {
    l1 = {line = line1; lineno = lineno1; matching_on = Vector.create ()};
    l2 = {line = line2; lineno = lineno2; matching_on = Vector.create ()};
    sim = similarity line1 line2;
}


(* True if elem is within the given inclusive range *)
let in_range elem (start, stop) =
  elem >= start && elem <= stop


(* True if the sim s should be hidden from the program's output *)
let should_exclude s lower_threshhold upper_threshhold exclude_pattern =
  not (in_range s.sim (lower_threshhold, upper_threshhold))
  || Str.string_match exclude_pattern s.l1.line 0
  || Str.string_match exclude_pattern s.l2.line 0


(* Traverse through the Vector of lines creating sims for every
 * line combination and sort by highest similarity percentage. *)
let analyze lines lower_threshhold upper_threshhold exclude_pattern =
  let v = Vector.create () in
  for i = 0 to Vector.length lines do
    for j = i + 1 to Vector.length lines do
      let similarity = find_similarities (Vector.get lines i) (Vector.get lines j) i j in
      if should_exclude similarity lower_threshhold upper_threshhold exclude_pattern then ()
      else Vector.push v similarity
    done
  done;
  Vector.sort (fun v1 v2 -> compare v1.sim v2.sim) v;
  v


(* How many digits are in this integer? *)
let int_len i = float_of_int i |> log10 |> ceil |> int_of_float


(* Create a linenumber str for lineno1 in the form "number:  ", ensuring:
 * len (lineno_str lineno1 lineno2) = len (lineno_str lineno2 lineno1) *)
let lineno_str lineno1 lineno2 =
  let len = max (int_len lineno1) (int_len lineno2) + 1 in
  let s = string_of_int lineno1 ^ ":" in
  s ^ String.make (2 + len - String.length s) ' '


(* Output the given sim to stdout *)
let print_sim s =
  Printf.printf "Line %d is %.2f%% similar to line %d\n" s.l1.lineno s.sim s.l2.lineno;
  let lineno_str1 = lineno_str s.l1.lineno s.l2.lineno in
  let lineno_str2 = lineno_str s.l2.lineno s.l1.lineno in
  let print_line line prepend =
    print_string prepend;
    String.iteri (fun i c ->
      if Vector.any (fun r -> in_range i r) s.l1.matching_on then
        Printf.printf "\033[0;31m%c\033[0;0m" c
      else print_char c
    ) line; print_endline ""
  in print_line s.l1.line lineno_str1;
  print_line s.l2.line lineno_str2;
  print_endline ""


let print_help () =
  print_endline "usage: simchk <filename> [lower-threshhold] [upper-threshhold] [exclude-pattern]";
  print_endline "";
  print_endline "lower-threshhold (0-100): exclude line pairs with less similarites than this amount";
  print_endline "upper-threshhold (0-100): exclude line pairs with more similarites than this amount";
  print_endline "exclude-pattern (regex): exclude lines containing this regex"


(* Return a string Vector.t of the lines of the given file *)
let lines_of_file filename =
  let file = filename |> open_in |> file_stream in
  let v = Vector.create () in
  Stream.iter (fun l -> Vector.push v l) file;
  v


(* Main entry point *)
let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    print_help ()
  else
    let v = lines_of_file Sys.argv.(1) in
    let lower_threshhold = if argc > 2 then float_of_string Sys.argv.(2) else default_lower_threshhold in
    let upper_threshhold = if argc > 3 then float_of_string Sys.argv.(3) else default_upper_threshhold in
    let exclude_pattern = if argc > 4 then Str.regexp (".*" ^ Sys.argv.(4) ^ ".*") else default_exclude_pattern in
    analyze v lower_threshhold upper_threshhold exclude_pattern
    |> Vector.iter print_sim


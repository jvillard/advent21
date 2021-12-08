let example =
  {|be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce|}

let assert_digit digit =
  String.iter (function 'a' .. 'g' -> () | _ -> raise (Invalid_argument digit)) digit

let is_determined digit =
  (* 1, 4, 7, 8 are uniquely determined by their number of segments on *)
  List.mem (String.length digit) [2; 4; 3; 7]

let part1 raw_notes =
  let displays =
    String.split_on_char '\n' raw_notes
    |> List.map (fun raw_note ->
           let[@warning "-8"] [_; raw_displays] = String.split_on_char '|' raw_note in
           let displays = String.trim raw_displays |> String.split_on_char ' ' in
           List.iter assert_digit displays ; displays )
  in
  let n =
    List.fold_left
      (fun sum display ->
        List.fold_left (fun sum digit -> if is_determined digit then sum + 1 else sum) sum display
        )
      0 displays
  in
  Format.printf "number of 1, 4, 7, 8 in displays: %d@\n" n

let () = part1 example

let input =
  let file = open_in "input" in
  let contents = really_input_string file (in_channel_length file) in
  close_in file ; contents

let () = part1 input

(** {Part 2} *)

type segment = A | B | C | D | E | F | G

module Segment = struct
  type t = segment

  let compare = compare
end

module SegMap = Map.Make (Segment)
module SegSet = Set.Make (Segment)

let all_segments = SegSet.of_list [A; B; C; D; E; F; G]

let to_segments s =
  String.to_seq s
  |> Seq.fold_left
       (fun segments c ->
         let segment =
           match c with
           | 'a' ->
               A
           | 'b' ->
               B
           | 'c' ->
               C
           | 'd' ->
               D
           | 'e' ->
               E
           | 'f' ->
               F
           | 'g' ->
               G
           | _ ->
               raise (Invalid_argument s)
         in
         SegSet.add segment segments )
       SegSet.empty

let parse raw_notes =
  String.split_on_char '\n' raw_notes
  |> List.map (fun raw_note ->
         let[@warning "-8"] [raw_all_digits; raw_display] = String.split_on_char '|' raw_note in
         let all_digits =
           String.trim raw_all_digits |> String.split_on_char ' ' |> List.map to_segments
         in
         let display =
           String.trim raw_display |> String.split_on_char ' ' |> List.map to_segments
         in
         (all_digits, display) )

(*
digits by number of segments:
2 -> 1
4 -> 4
3 -> 7
7 -> 8
5 -> 2, 3, 5
6 -> 0, 6, 9

5-segment digits all have a, d, g in common, only d in common with 4
-> we know d
other two ones in common are a, g, only a in common with 7
-> we know a
-> we know g (remaining one)

segments in 4 minus segments in 1 + we know d
-> we know b

a,b,d,g
segments common to all six-segments are abfg
-> unknown segment is f
-> we know c: only unknown segment in 4
a,b,c,d,f,g known, e is the only remaining one
 *)
let decoder all_digits =
  let find_by_length length = List.find (fun s -> SegSet.cardinal s = length) all_digits in
  let find_all_by_length length = List.filter (fun s -> SegSet.cardinal s = length) all_digits in
  let minus segments l =
    List.fold_left (fun segments segment -> SegSet.remove segment segments) segments l
  in
  let one_segment segments =
    assert (SegSet.cardinal segments = 1) ;
    SegSet.min_elt segments
  in
  let one = find_by_length 2 in
  let four = find_by_length 4 in
  let seven = find_by_length 3 in
  let adg = List.fold_left SegSet.inter all_segments (find_all_by_length 5) in
  let d = SegSet.inter adg four |> one_segment in
  let ag = SegSet.remove d adg in
  let a = SegSet.inter ag seven |> one_segment in
  let g = SegSet.remove a ag |> one_segment in
  let b = SegSet.diff four one |> SegSet.remove d |> one_segment in
  let f =
    let abfg = List.fold_left SegSet.inter all_segments (find_all_by_length 6) in
    minus abfg [a; b; g] |> one_segment
  in
  let c = minus four [b; d; f] |> one_segment in
  let e = minus all_segments [a; b; c; d; f; g] |> one_segment in
  SegMap.empty |> SegMap.add a A |> SegMap.add b B |> SegMap.add c C |> SegMap.add d D
  |> SegMap.add e E |> SegMap.add f F |> SegMap.add g G

module SegSetMap = Map.Make (SegSet)

let segment_map =
  let add segments i map =
    let set = SegSet.of_list segments in
    SegSetMap.add set i map
  in
  SegSetMap.empty
  |> add [A; B; C; E; F; G] 0
  |> add [C; F] 1
  |> add [A; C; D; E; G] 2
  |> add [A; C; D; F; G] 3
  |> add [B; C; D; F] 4
  |> add [A; B; D; F; G] 5
  |> add [A; B; D; E; F; G] 6
  |> add [A; C; F] 7
  |> add [A; B; C; D; E; F; G] 8
  |> add [A; B; C; D; F; G] 9

let int_of_segments digit = SegSetMap.find digit segment_map

let decode_digit decoder digit =
  SegSet.map (fun seg -> SegMap.find seg decoder) digit |> int_of_segments

let decode_display decoder display =
  List.fold_left (fun number digit -> (number * 10) + decode_digit decoder digit) 0 display

let part2 raw_notes =
  parse raw_notes
  |> List.fold_left
       (fun sum (all_digits, display) -> sum + decode_display (decoder all_digits) display)
       0
  |> Format.printf "Part 2: sum=%d@\n"

let () = part2 example ; part2 input

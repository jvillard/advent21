let example = [3; 4; 3; 1; 2]

(** [7 - 1] *)
let fish_clock = 6

let one_round fish =
  let new_fish = ref 0 in
  let fish' =
    (* List.map stack overflows lol *)
    List.rev_map (fun fish -> if fish > 0 then fish - 1 else (incr new_fish ; fish_clock)) fish
  in
  List.rev_append (List.init !new_fish (fun _ -> fish_clock + 2)) fish'

let rec go_forth ~time fish = if time > 0 then one_round fish |> go_forth ~time:(time - 1) else fish

let main ~time fish =
  let so_many_fish = go_forth ~time fish in
  Format.printf "n_days= %d; final population= %d@\n" time (List.length so_many_fish)

let () = Format.printf "example@\n" ; main ~time:18 example ; main ~time:80 example

let input =
  [ 1
  ; 1
  ; 1
  ; 2
  ; 1
  ; 1
  ; 2
  ; 1
  ; 1
  ; 1
  ; 5
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 2
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 3
  ; 1
  ; 1
  ; 3
  ; 1
  ; 1
  ; 1
  ; 4
  ; 1
  ; 5
  ; 1
  ; 3
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 5
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 5
  ; 5
  ; 2
  ; 5
  ; 1
  ; 1
  ; 2
  ; 1
  ; 1
  ; 1
  ; 1
  ; 3
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 2
  ; 1
  ; 1
  ; 1
  ; 1
  ; 5
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 5
  ; 1
  ; 2
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 3
  ; 3
  ; 2
  ; 1
  ; 1
  ; 4
  ; 1
  ; 1
  ; 5
  ; 5
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 2
  ; 5
  ; 1
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 2
  ; 1
  ; 1
  ; 5
  ; 2
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 4
  ; 3
  ; 1
  ; 1
  ; 3
  ; 1
  ; 3
  ; 1
  ; 4
  ; 1
  ; 5
  ; 4
  ; 1
  ; 1
  ; 2
  ; 1
  ; 1
  ; 5
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 5
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 4
  ; 1
  ; 1
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 5
  ; 4
  ; 1
  ; 2
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 3
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 4
  ; 1
  ; 1
  ; 1
  ; 2
  ; 1
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 4
  ; 2
  ; 1
  ; 2
  ; 1
  ; 1
  ; 4
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 3
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 3
  ; 2
  ; 1
  ; 4
  ; 1
  ; 5
  ; 1
  ; 1
  ; 1
  ; 4
  ; 5
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 1
  ; 5
  ; 1
  ; 1
  ; 5
  ; 1
  ; 2
  ; 1
  ; 1
  ; 2
  ; 4
  ; 1
  ; 1
  ; 2
  ; 1
  ; 5
  ; 5
  ; 3 ]

let () =
  Format.printf "@\npuzzle input@\n" ;
  main ~time:80 input

(** {Part 2} ok I need to be less dumb... *)

(* how many fish are which number of days away from giving birth, only 9 possible values *)
let mk_fish population0 =
  let fish = Array.make 9 0 in
  List.iter (fun i -> fish.(i) <- fish.(i) + 1) population0 ;
  fish

let one_round fish =
  let new_fish = fish.(0) in
  for i = 0 to 7 do
    fish.(i) <- fish.(i + 1)
  done ;
  fish.(8) <- new_fish ;
  fish.(6) <- fish.(6) + new_fish

let rec go_forth ~time fish =
  if time > 0 then (
    one_round fish ;
    go_forth ~time:(time - 1) fish )

let count fish =
  let count = ref 0 in
  Array.iter (fun n -> count := !count + n) fish ;
  !count

let main ~time fish =
  let fish = mk_fish fish in
  go_forth ~time fish ;
  Format.printf "final population= %d@\n" (count fish)

let () =
  Format.printf "@\nPart 2 -- example@\n" ;
  main ~time:18 example ;
  main ~time:80 example ;
  main ~time:256 example ;
  Format.printf "@\nPart 2 -- puzzle input@\n" ;
  main ~time:256 input ;
  ()

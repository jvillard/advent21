type direction = Forward of int | Down of int | Up of int

type position = {h: int; v: int}

let initial_pos = {h= 0; v= 0}

let pp_position fmt {h; v} = Format.fprintf fmt "{h=%d; v=%d}" h v

let hash {h; v} = h * v

let move_one {h= h0; v= v0} move =
  match move with
  | Forward x ->
      {h= h0 + x; v= v0}
  | Down x ->
      {h= h0; v= v0 + x}
  | Up x ->
      {h= h0; v= v0 - x}

let move moves = List.fold_left move_one initial_pos moves

let example = [Forward 5; Down 5; Forward 8; Up 3; Down 8; Forward 2]

let () =
  let final_example = move example in
  Format.printf "final position (example): %a; result= %d@." pp_position final_example
    (hash final_example)

let input =
  [ Forward 4
  ; Down 8
  ; Down 3
  ; Down 1
  ; Forward 8
  ; Up 6
  ; Down 4
  ; Forward 2
  ; Down 4
  ; Down 6
  ; Down 7
  ; Forward 1
  ; Down 4
  ; Down 6
  ; Forward 7
  ; Down 2
  ; Up 8
  ; Up 3
  ; Forward 1
  ; Forward 2
  ; Down 3
  ; Down 8
  ; Forward 6
  ; Forward 5
  ; Down 4
  ; Down 1
  ; Up 5
  ; Down 5
  ; Down 2
  ; Up 6
  ; Forward 4
  ; Forward 3
  ; Down 8
  ; Down 9
  ; Up 2
  ; Forward 1
  ; Forward 2
  ; Down 1
  ; Forward 3
  ; Down 7
  ; Up 6
  ; Down 1
  ; Down 7
  ; Down 5
  ; Forward 8
  ; Down 5
  ; Down 1
  ; Down 7
  ; Up 9
  ; Forward 6
  ; Up 8
  ; Down 3
  ; Down 9
  ; Down 3
  ; Forward 2
  ; Forward 1
  ; Forward 4
  ; Down 7
  ; Up 8
  ; Down 1
  ; Up 1
  ; Forward 4
  ; Down 7
  ; Forward 5
  ; Forward 2
  ; Forward 1
  ; Up 8
  ; Down 2
  ; Up 6
  ; Down 7
  ; Down 4
  ; Up 6
  ; Forward 2
  ; Forward 8
  ; Down 8
  ; Down 2
  ; Forward 2
  ; Forward 9
  ; Down 1
  ; Forward 5
  ; Down 4
  ; Forward 4
  ; Down 2
  ; Down 1
  ; Forward 7
  ; Down 1
  ; Down 5
  ; Down 5
  ; Up 5
  ; Forward 7
  ; Forward 6
  ; Forward 3
  ; Forward 9
  ; Forward 3
  ; Forward 5
  ; Down 8
  ; Down 9
  ; Forward 7
  ; Up 5
  ; Up 7
  ; Down 5
  ; Up 9
  ; Forward 9
  ; Up 8
  ; Up 2
  ; Forward 5
  ; Down 2
  ; Forward 2
  ; Down 4
  ; Up 4
  ; Down 2
  ; Up 3
  ; Up 8
  ; Down 3
  ; Down 4
  ; Down 7
  ; Forward 3
  ; Forward 9
  ; Down 1
  ; Down 2
  ; Down 5
  ; Down 1
  ; Forward 2
  ; Forward 2
  ; Up 8
  ; Down 4
  ; Forward 7
  ; Up 6
  ; Down 9
  ; Down 6
  ; Up 1
  ; Down 2
  ; Forward 6
  ; Down 4
  ; Up 1
  ; Forward 3
  ; Down 4
  ; Down 1
  ; Up 8
  ; Forward 3
  ; Down 5
  ; Up 2
  ; Down 8
  ; Down 4
  ; Up 2
  ; Down 2
  ; Forward 6
  ; Up 4
  ; Up 2
  ; Down 2
  ; Forward 7
  ; Down 5
  ; Forward 2
  ; Forward 8
  ; Up 3
  ; Forward 5
  ; Up 6
  ; Down 4
  ; Down 1
  ; Down 8
  ; Down 2
  ; Forward 8
  ; Up 2
  ; Down 5
  ; Up 8
  ; Down 1
  ; Down 1
  ; Down 5
  ; Up 4
  ; Down 1
  ; Down 3
  ; Down 8
  ; Forward 6
  ; Down 9
  ; Forward 6
  ; Up 2
  ; Forward 1
  ; Forward 9
  ; Down 9
  ; Down 3
  ; Down 9
  ; Down 6
  ; Down 4
  ; Down 8
  ; Forward 1
  ; Down 1
  ; Forward 2
  ; Up 2
  ; Forward 8
  ; Down 1
  ; Up 6
  ; Down 4
  ; Down 3
  ; Forward 8
  ; Up 7
  ; Down 6
  ; Down 1
  ; Down 2
  ; Forward 1
  ; Up 5
  ; Up 7
  ; Down 6
  ; Down 4
  ; Down 5
  ; Forward 9
  ; Down 7
  ; Down 9
  ; Down 5
  ; Forward 9
  ; Forward 7
  ; Forward 9
  ; Forward 8
  ; Up 4
  ; Forward 5
  ; Down 7
  ; Forward 8
  ; Up 1
  ; Forward 3
  ; Forward 2
  ; Forward 2
  ; Down 7
  ; Forward 9
  ; Down 7
  ; Down 9
  ; Forward 6
  ; Forward 8
  ; Up 5
  ; Up 8
  ; Up 7
  ; Up 6
  ; Forward 7
  ; Down 6
  ; Down 5
  ; Down 3
  ; Forward 7
  ; Down 7
  ; Forward 6
  ; Down 4
  ; Down 2
  ; Down 9
  ; Down 2
  ; Up 8
  ; Down 8
  ; Down 3
  ; Down 4
  ; Forward 3
  ; Up 6
  ; Down 9
  ; Forward 1
  ; Down 3
  ; Forward 9
  ; Down 6
  ; Forward 9
  ; Forward 8
  ; Forward 5
  ; Up 2
  ; Forward 5
  ; Up 7
  ; Down 6
  ; Forward 6
  ; Down 8
  ; Forward 2
  ; Down 7
  ; Down 8
  ; Up 1
  ; Forward 3
  ; Forward 5
  ; Down 3
  ; Forward 8
  ; Up 7
  ; Forward 9
  ; Forward 6
  ; Forward 1
  ; Forward 7
  ; Down 5
  ; Forward 3
  ; Down 5
  ; Down 6
  ; Down 7
  ; Down 3
  ; Down 8
  ; Up 5
  ; Forward 2
  ; Forward 5
  ; Up 7
  ; Up 4
  ; Forward 1
  ; Forward 1
  ; Down 1
  ; Down 7
  ; Forward 4
  ; Up 8
  ; Forward 5
  ; Down 9
  ; Up 7
  ; Forward 8
  ; Down 4
  ; Forward 4
  ; Forward 6
  ; Down 8
  ; Forward 7
  ; Down 1
  ; Forward 9
  ; Down 9
  ; Up 1
  ; Down 6
  ; Forward 6
  ; Down 7
  ; Down 4
  ; Forward 6
  ; Forward 3
  ; Down 5
  ; Up 5
  ; Up 7
  ; Up 5
  ; Down 6
  ; Forward 7
  ; Up 3
  ; Down 2
  ; Forward 6
  ; Down 8
  ; Down 7
  ; Up 9
  ; Forward 3
  ; Forward 1
  ; Down 8
  ; Forward 6
  ; Forward 4
  ; Up 9
  ; Forward 3
  ; Down 1
  ; Forward 4
  ; Forward 9
  ; Forward 2
  ; Forward 8
  ; Forward 1
  ; Forward 2
  ; Forward 7
  ; Down 6
  ; Forward 6
  ; Up 2
  ; Forward 5
  ; Up 8
  ; Down 9
  ; Up 8
  ; Down 5
  ; Down 1
  ; Down 6
  ; Up 4
  ; Down 4
  ; Down 5
  ; Up 6
  ; Down 8
  ; Down 8
  ; Forward 9
  ; Forward 8
  ; Forward 2
  ; Down 2
  ; Up 3
  ; Forward 2
  ; Down 8
  ; Down 8
  ; Forward 3
  ; Forward 5
  ; Down 9
  ; Down 2
  ; Forward 6
  ; Forward 7
  ; Down 7
  ; Forward 4
  ; Forward 2
  ; Down 1
  ; Down 6
  ; Up 5
  ; Down 2
  ; Forward 3
  ; Forward 9
  ; Down 9
  ; Down 3
  ; Forward 3
  ; Forward 6
  ; Down 2
  ; Forward 5
  ; Forward 7
  ; Down 6
  ; Forward 4
  ; Down 6
  ; Forward 6
  ; Forward 3
  ; Forward 3
  ; Forward 8
  ; Down 4
  ; Up 4
  ; Down 6
  ; Down 4
  ; Down 9
  ; Forward 7
  ; Forward 4
  ; Forward 7
  ; Down 3
  ; Forward 1
  ; Down 7
  ; Down 3
  ; Forward 1
  ; Down 7
  ; Down 5
  ; Forward 6
  ; Up 7
  ; Down 7
  ; Forward 5
  ; Forward 5
  ; Up 9
  ; Down 7
  ; Forward 1
  ; Forward 2
  ; Down 4
  ; Down 8
  ; Down 7
  ; Forward 4
  ; Forward 4
  ; Forward 3
  ; Down 8
  ; Down 7
  ; Down 8
  ; Forward 2
  ; Down 2
  ; Forward 2
  ; Forward 4
  ; Up 6
  ; Down 4
  ; Up 3
  ; Forward 7
  ; Down 9
  ; Down 3
  ; Forward 3
  ; Down 2
  ; Down 2
  ; Up 5
  ; Down 4
  ; Forward 3
  ; Forward 3
  ; Up 7
  ; Forward 8
  ; Forward 6
  ; Down 3
  ; Forward 2
  ; Down 6
  ; Up 1
  ; Down 7
  ; Down 7
  ; Forward 8
  ; Up 1
  ; Up 8
  ; Up 4
  ; Up 1
  ; Forward 4
  ; Forward 9
  ; Down 9
  ; Down 5
  ; Down 3
  ; Forward 8
  ; Down 3
  ; Forward 4
  ; Down 6
  ; Down 9
  ; Down 3
  ; Forward 6
  ; Up 1
  ; Up 4
  ; Forward 9
  ; Down 3
  ; Up 1
  ; Forward 4
  ; Up 1
  ; Forward 8
  ; Down 9
  ; Up 1
  ; Forward 3
  ; Down 7
  ; Down 7
  ; Down 3
  ; Forward 7
  ; Forward 5
  ; Down 8
  ; Up 8
  ; Down 6
  ; Down 4
  ; Forward 9
  ; Down 9
  ; Up 5
  ; Forward 6
  ; Down 8
  ; Up 8
  ; Down 2
  ; Forward 1
  ; Down 8
  ; Down 2
  ; Forward 7
  ; Forward 2
  ; Down 2
  ; Forward 5
  ; Up 2
  ; Down 6
  ; Down 1
  ; Down 6
  ; Down 3
  ; Up 4
  ; Forward 4
  ; Forward 8
  ; Down 3
  ; Forward 9
  ; Forward 6
  ; Down 2
  ; Up 2
  ; Down 2
  ; Up 4
  ; Down 8
  ; Forward 5
  ; Down 4
  ; Forward 3
  ; Down 4
  ; Forward 6
  ; Down 8
  ; Down 2
  ; Up 7
  ; Down 3
  ; Down 6
  ; Up 1
  ; Forward 8
  ; Up 5
  ; Down 1
  ; Forward 3
  ; Down 2
  ; Down 5
  ; Up 5
  ; Up 2
  ; Down 2
  ; Down 2
  ; Down 4
  ; Forward 3
  ; Up 7
  ; Forward 8
  ; Forward 4
  ; Down 3
  ; Forward 8
  ; Down 4
  ; Down 9
  ; Down 7
  ; Up 3
  ; Up 4
  ; Down 4
  ; Forward 3
  ; Down 3
  ; Up 5
  ; Down 1
  ; Forward 4
  ; Forward 9
  ; Forward 3
  ; Forward 3
  ; Up 6
  ; Down 3
  ; Forward 3
  ; Up 7
  ; Down 3
  ; Up 7
  ; Up 2
  ; Up 2
  ; Down 9
  ; Forward 4
  ; Forward 7
  ; Forward 7
  ; Down 7
  ; Forward 2
  ; Forward 1
  ; Down 9
  ; Forward 2
  ; Down 2
  ; Down 4
  ; Up 3
  ; Forward 8
  ; Up 3
  ; Down 7
  ; Forward 9
  ; Down 7
  ; Forward 2
  ; Down 1
  ; Up 9
  ; Forward 7
  ; Forward 9
  ; Up 4
  ; Forward 3
  ; Forward 1
  ; Down 5
  ; Down 6
  ; Forward 9
  ; Down 9
  ; Forward 2
  ; Forward 8
  ; Forward 4
  ; Forward 9
  ; Down 5
  ; Down 9
  ; Down 3
  ; Down 7
  ; Up 2
  ; Up 7
  ; Forward 6
  ; Down 3
  ; Down 2
  ; Up 1
  ; Forward 4
  ; Down 1
  ; Up 4
  ; Up 8
  ; Down 9
  ; Down 5
  ; Down 7
  ; Forward 4
  ; Down 1
  ; Forward 8
  ; Down 5
  ; Forward 7
  ; Down 3
  ; Up 2
  ; Forward 4
  ; Down 1
  ; Forward 4
  ; Up 5
  ; Forward 9
  ; Down 1
  ; Forward 7
  ; Up 3
  ; Up 9
  ; Forward 4
  ; Up 5
  ; Down 6
  ; Forward 2
  ; Down 1
  ; Forward 1
  ; Down 9
  ; Forward 5
  ; Down 2
  ; Up 3
  ; Down 5
  ; Up 4
  ; Down 5
  ; Down 8
  ; Down 8
  ; Down 3
  ; Forward 9
  ; Forward 2
  ; Down 3
  ; Down 3
  ; Down 6
  ; Down 8
  ; Forward 9
  ; Down 4
  ; Down 1
  ; Forward 4
  ; Down 9
  ; Forward 1
  ; Down 9
  ; Up 6
  ; Up 7
  ; Up 8
  ; Forward 5
  ; Down 3
  ; Up 5
  ; Up 1
  ; Down 8
  ; Forward 1
  ; Forward 7
  ; Up 9
  ; Down 7
  ; Forward 4
  ; Down 5
  ; Forward 2
  ; Down 6
  ; Up 8
  ; Down 1
  ; Down 6
  ; Down 9
  ; Down 8
  ; Forward 8
  ; Down 4
  ; Up 2
  ; Down 2
  ; Forward 9
  ; Up 6
  ; Down 3
  ; Forward 5
  ; Forward 9
  ; Up 2
  ; Up 5
  ; Down 5
  ; Forward 2
  ; Forward 3
  ; Forward 2
  ; Up 2
  ; Down 2
  ; Forward 9
  ; Up 4
  ; Down 4
  ; Up 1
  ; Down 1
  ; Down 6
  ; Down 6
  ; Forward 2
  ; Up 6
  ; Up 9
  ; Forward 7
  ; Forward 4
  ; Down 6
  ; Down 5
  ; Down 5
  ; Down 9
  ; Forward 7
  ; Down 1
  ; Up 5
  ; Forward 4
  ; Up 8
  ; Up 8
  ; Down 4
  ; Down 7
  ; Forward 1
  ; Forward 8
  ; Down 3
  ; Up 3
  ; Up 3
  ; Up 4
  ; Down 1
  ; Down 8
  ; Up 6
  ; Up 8
  ; Forward 2
  ; Down 2
  ; Down 3
  ; Forward 4
  ; Forward 3
  ; Forward 6
  ; Down 1
  ; Up 6
  ; Forward 2
  ; Forward 6
  ; Forward 2
  ; Forward 5
  ; Down 1
  ; Up 4
  ; Forward 7
  ; Down 6
  ; Forward 8
  ; Up 9
  ; Down 5
  ; Up 3
  ; Forward 8
  ; Forward 1
  ; Forward 9
  ; Up 9
  ; Forward 4
  ; Forward 5
  ; Down 1
  ; Up 9
  ; Down 5
  ; Down 7
  ; Forward 8
  ; Down 1
  ; Forward 3
  ; Forward 2
  ; Down 9
  ; Down 1
  ; Forward 5
  ; Up 6
  ; Down 7
  ; Forward 4
  ; Down 6
  ; Forward 1
  ; Forward 8
  ; Up 4
  ; Forward 5
  ; Down 8
  ; Forward 6
  ; Up 2
  ; Forward 3
  ; Forward 5
  ; Up 6
  ; Up 8
  ; Up 4
  ; Forward 6
  ; Down 2
  ; Down 6
  ; Down 5
  ; Up 2
  ; Down 3
  ; Down 7
  ; Up 6
  ; Forward 2
  ; Forward 3
  ; Up 6
  ; Forward 3
  ; Up 8
  ; Forward 6
  ; Down 8
  ; Down 7
  ; Down 1
  ; Down 6
  ; Up 8
  ; Up 9
  ; Down 4
  ; Forward 2
  ; Forward 7
  ; Down 8
  ; Up 6
  ; Up 8
  ; Up 8
  ; Down 4
  ; Forward 9
  ; Down 5
  ; Forward 5
  ; Forward 3
  ; Down 1
  ; Forward 1
  ; Up 9
  ; Down 1
  ; Down 6
  ; Up 6
  ; Forward 7
  ; Forward 1
  ; Down 5
  ; Down 2
  ; Forward 5
  ; Down 3
  ; Down 4
  ; Forward 6
  ; Up 6
  ; Down 9
  ; Up 3
  ; Forward 1
  ; Up 3
  ; Down 5
  ; Up 4
  ; Down 4
  ; Forward 9
  ; Up 5
  ; Down 1
  ; Forward 4
  ; Down 8
  ; Up 1
  ; Forward 9
  ; Forward 8
  ; Up 4
  ; Up 3
  ; Up 5
  ; Forward 5
  ; Up 7
  ; Forward 5
  ; Forward 4
  ; Forward 6
  ; Forward 9
  ; Down 6
  ; Down 3
  ; Up 5
  ; Forward 2
  ; Up 9
  ; Down 4
  ; Down 2
  ; Forward 5
  ; Up 6
  ; Forward 1
  ; Up 5
  ; Up 3
  ; Down 4
  ; Forward 3
  ; Forward 6
  ; Up 4
  ; Up 6
  ; Down 3
  ; Down 2
  ; Up 3
  ; Down 9
  ; Up 7
  ; Forward 6
  ; Up 4
  ; Forward 7
  ; Down 4
  ; Up 6
  ; Down 6
  ; Forward 9
  ; Forward 4
  ; Up 2
  ; Forward 7
  ; Up 5
  ; Forward 2
  ; Forward 2
  ; Down 4
  ; Down 4
  ; Forward 3
  ; Down 4
  ; Up 3
  ; Forward 9
  ; Down 5
  ; Forward 6
  ; Forward 9
  ; Forward 9
  ; Up 6
  ; Down 9
  ; Forward 8
  ; Up 7
  ; Up 5
  ; Down 6
  ; Forward 6
  ; Forward 1
  ; Down 6
  ; Forward 5
  ; Down 2
  ; Down 1
  ; Forward 6
  ; Down 6
  ; Down 9
  ; Down 5
  ; Forward 1
  ; Down 7
  ; Down 7
  ; Down 4
  ; Forward 7
  ; Up 5
  ; Up 1
  ; Up 2
  ; Up 5
  ; Down 3
  ; Forward 9
  ; Forward 2
  ; Forward 8
  ; Up 4
  ; Forward 7
  ; Forward 6
  ; Forward 9
  ; Down 2
  ; Down 6
  ; Forward 4
  ; Down 9
  ; Down 9
  ; Up 3
  ; Forward 2
  ; Forward 1
  ; Down 5
  ; Up 9
  ; Down 6
  ; Forward 6
  ; Down 8
  ; Forward 3
  ; Forward 5
  ; Forward 3
  ; Forward 2
  ; Down 7
  ; Down 2
  ; Up 8
  ; Forward 9
  ; Down 8
  ; Up 7
  ; Down 4
  ; Up 3
  ; Forward 6
  ; Down 3
  ; Up 3
  ; Down 6
  ; Down 3
  ; Up 2
  ; Down 4
  ; Down 4
  ; Up 2
  ; Down 6
  ; Down 5
  ; Down 9
  ; Down 1
  ; Down 7
  ; Up 9
  ; Down 4
  ; Up 6
  ; Down 6
  ; Forward 9
  ; Forward 2
  ; Down 8
  ; Down 3
  ; Forward 4
  ; Forward 4
  ; Forward 5
  ; Down 2
  ; Down 8
  ; Down 1
  ; Up 4
  ; Forward 9
  ; Up 7
  ; Forward 5
  ; Down 5
  ; Up 9
  ; Down 2
  ; Down 2
  ; Forward 4
  ; Forward 4
  ; Forward 8 ]

let () =
  let final = move input in
  Format.printf "final position (input): %a; result= %d@." pp_position final (hash final)
type t = float array array

let create rows cols init =
  Array.make_matrix rows cols init

let rows m = Array.length m

let cols m =
  if Array.length m = 0 then 0
  else Array.length m.(0)

let get m i j = m.(i).(j)

let set m i j v = m.(i).(j) <- v

let copy m =
  Array.map Array.copy m

let of_array arr =
  Array.map Array.copy arr

let to_array m =
  Array.map Array.copy m

let map f m =
  Array.map (Array.map f) m

let mapi f m =
  Array.mapi (fun i row -> Array.mapi (fun j v -> f i j v) row) m

let fold f init m =
  Array.fold_left (fun acc row -> Array.fold_left f acc row) init m

let equal m1 m2 =
  let r1, c1 = rows m1, cols m1 in
  let r2, c2 = rows m2, cols m2 in
  if r1 <> r2 || c1 <> c2 then false
  else
    try
      for i = 0 to r1 - 1 do
        for j = 0 to c1 - 1 do
          if m1.(i).(j) <> m2.(i).(j) then raise Exit
        done
      done;
      true
    with Exit -> false

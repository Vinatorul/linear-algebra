type orientation = Row | Column

type t = {
  data : Matrix.t;
  orientation : orientation;
}

let create orientation n init =
  let data = match orientation with
    | Row -> Matrix.create 1 n init
    | Column -> Matrix.create n 1 init
  in
  { data; orientation }

let length v =
  match v.orientation with
  | Row -> Matrix.cols v.data
  | Column -> Matrix.rows v.data

let get v i =
  match v.orientation with
  | Row -> Matrix.get v.data 0 i
  | Column -> Matrix.get v.data i 0

let set v i value =
  match v.orientation with
  | Row -> Matrix.set v.data 0 i value
  | Column -> Matrix.set v.data i 0 value

let copy v =
  { data = Matrix.copy v.data; orientation = v.orientation }

let of_array orientation arr =
  let n = Array.length arr in
  let data = match orientation with
    | Row -> Array.make_matrix 1 n 0.0
    | Column -> Array.make_matrix n 1 0.0
  in
  for i = 0 to n - 1 do
    match orientation with
    | Row -> data.(0).(i) <- arr.(i)
    | Column -> data.(i).(0) <- arr.(i)
  done;
  { data; orientation }

let to_array v =
  let n = length v in
  Array.init n (fun i -> get v i)

let map f v =
  { data = Matrix.map f v.data; orientation = v.orientation }

let mapi f v =
  let n = length v in
  let result = create v.orientation n 0.0 in
  for i = 0 to n - 1 do
    set result i (f i (get v i))
  done;
  result

let fold f init v =
  Matrix.fold f init v.data

let equal v1 v2 =
  v1.orientation = v2.orientation && Matrix.equal v1.data v2.data

let to_matrix v = v.data

let of_matrix m orientation =
  let rows = Matrix.rows m in
  let cols = Matrix.cols m in
  match orientation with
  | Row ->
      if rows <> 1 then
        invalid_arg "of_matrix: matrix must have 1 row for Row orientation"
      else
        { data = m; orientation }
  | Column ->
      if cols <> 1 then
        invalid_arg "of_matrix: matrix must have 1 column for Column orientation"
      else
        { data = m; orientation }

let orientation v = v.orientation

let dot v1 v2 =
  if length v1 <> length v2 then
    invalid_arg "dot: vectors must have the same length";
  let sum = ref 0.0 in
  for i = 0 to length v1 - 1 do
    sum := !sum +. (get v1 i) *. (get v2 i)
  done;
  !sum

let add v1 v2 =
  if v1.orientation <> v2.orientation then
    invalid_arg "add: vectors must have the same orientation";
  if length v1 <> length v2 then
    invalid_arg "add: vectors must have the same length";
  let n = length v1 in
  let result = create v1.orientation n 0.0 in
  for i = 0 to n - 1 do
    set result i (get v1 i +. get v2 i)
  done;
  result

let sub v1 v2 =
  if v1.orientation <> v2.orientation then
    invalid_arg "sub: vectors must have the same orientation";
  if length v1 <> length v2 then
    invalid_arg "sub: vectors must have the same length";
  let n = length v1 in
  let result = create v1.orientation n 0.0 in
  for i = 0 to n - 1 do
    set result i (get v1 i -. get v2 i)
  done;
  result

let scale k v =
  map (fun x -> k *. x) v
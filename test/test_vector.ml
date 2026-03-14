open Linear_algebra

let test_create_row () =
  let v = Vector.create Vector.Row 3 1.0 in
  Alcotest.(check int) "length" 3 (Vector.length v);
  Alcotest.(check (float 0.0)) "element 0" 1.0 (Vector.get v 0);
  Alcotest.(check (float 0.0)) "element 1" 1.0 (Vector.get v 1);
  Alcotest.(check (float 0.0)) "element 2" 1.0 (Vector.get v 2)

let test_create_column () =
  let v = Vector.create Vector.Column 3 2.0 in
  Alcotest.(check int) "length" 3 (Vector.length v);
  Alcotest.(check (float 0.0)) "element 0" 2.0 (Vector.get v 0);
  Alcotest.(check (float 0.0)) "element 1" 2.0 (Vector.get v 1);
  Alcotest.(check (float 0.0)) "element 2" 2.0 (Vector.get v 2)

let test_get_set () =
  let v = Vector.create Vector.Row 3 0.0 in
  Vector.set v 0 1.0;
  Vector.set v 1 2.0;
  Vector.set v 2 3.0;
  Alcotest.(check (float 0.0)) "element 0" 1.0 (Vector.get v 0);
  Alcotest.(check (float 0.0)) "element 1" 2.0 (Vector.get v 1);
  Alcotest.(check (float 0.0)) "element 2" 3.0 (Vector.get v 2)

let test_get_set_column () =
  let v = Vector.create Vector.Column 3 0.0 in
  Vector.set v 0 1.0;
  Vector.set v 1 2.0;
  Vector.set v 2 3.0;
  Alcotest.(check (float 0.0)) "element 0" 1.0 (Vector.get v 0);
  Alcotest.(check (float 0.0)) "element 1" 2.0 (Vector.get v 1);
  Alcotest.(check (float 0.0)) "element 2" 3.0 (Vector.get v 2)

let test_copy () =
  let v1 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.copy v1 in
  Vector.set v2 0 10.0;
  Alcotest.(check (float 0.0)) "original unchanged" 1.0 (Vector.get v1 0);
  Alcotest.(check (float 0.0)) "copy changed" 10.0 (Vector.get v2 0)

let test_of_array_to_array () =
  let arr = [|1.0; 2.0; 3.0|] in
  let v = Vector.of_array Vector.Column arr in
  let arr2 = Vector.to_array v in
  Alcotest.(check (array (float 0.0))) "arrays equal" arr arr2

let test_map () =
  let v = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.map (fun x -> x *. 2.0) v in
  Alcotest.(check (float 0.0)) "element 0" 2.0 (Vector.get v2 0);
  Alcotest.(check (float 0.0)) "element 1" 4.0 (Vector.get v2 1);
  Alcotest.(check (float 0.0)) "element 2" 6.0 (Vector.get v2 2)

let test_mapi () =
  let v = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.mapi (fun i x -> float_of_int i +. x) v in
  Alcotest.(check (float 0.0)) "element 0" 1.0 (Vector.get v2 0);
  Alcotest.(check (float 0.0)) "element 1" 3.0 (Vector.get v2 1);
  Alcotest.(check (float 0.0)) "element 2" 5.0 (Vector.get v2 2)

let test_fold () =
  let v = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let sum = Vector.fold (fun acc x -> acc +. x) 0.0 v in
  Alcotest.(check (float 0.0)) "sum" 6.0 sum

let test_equal () =
  let v1 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v3 = Vector.of_array Vector.Row [|1.0; 2.0; 4.0|] in
  let v4 = Vector.of_array Vector.Column [|1.0; 2.0; 3.0|] in
  Alcotest.(check bool) "equal vectors" true (Vector.equal v1 v2);
  Alcotest.(check bool) "different values" false (Vector.equal v1 v3);
  Alcotest.(check bool) "different orientation" false (Vector.equal v1 v4)

let test_to_matrix () =
  let v_row = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let m_row = Vector.to_matrix v_row in
  Alcotest.(check int) "row matrix rows" 1 (Matrix.rows m_row);
  Alcotest.(check int) "row matrix cols" 3 (Matrix.cols m_row);
  
  let v_col = Vector.of_array Vector.Column [|1.0; 2.0; 3.0|] in
  let m_col = Vector.to_matrix v_col in
  Alcotest.(check int) "column matrix rows" 3 (Matrix.rows m_col);
  Alcotest.(check int) "column matrix cols" 1 (Matrix.cols m_col)

let test_of_matrix () =
  let m_row = Matrix.of_array [|[|1.0; 2.0; 3.0|]|] in
  let v_row = Vector.of_matrix m_row Vector.Row in
  Alcotest.(check int) "row vector length" 3 (Vector.length v_row);
  
  let m_col = Matrix.of_array [|[|1.0|]; [|2.0|]; [|3.0|]|] in
  let v_col = Vector.of_matrix m_col Vector.Column in
  Alcotest.(check int) "column vector length" 3 (Vector.length v_col)

let test_of_matrix_invalid_row () =
  let m = Matrix.of_array [|[|1.0; 2.0|]; [|3.0; 4.0|]|] in
  Alcotest.check_raises "invalid row matrix"
    (Invalid_argument "of_matrix: matrix must have 1 row for Row orientation")
    (fun () -> ignore (Vector.of_matrix m Vector.Row))

let test_of_matrix_invalid_column () =
  let m = Matrix.of_array [|[|1.0; 2.0|]; [|3.0; 4.0|]|] in
  Alcotest.check_raises "invalid column matrix"
    (Invalid_argument "of_matrix: matrix must have 1 column for Column orientation")
    (fun () -> ignore (Vector.of_matrix m Vector.Column))

let test_orientation () =
  let v_row = Vector.create Vector.Row 3 0.0 in
  let v_col = Vector.create Vector.Column 3 0.0 in
  Alcotest.(check bool) "row orientation" true (Vector.orientation v_row = Vector.Row);
  Alcotest.(check bool) "column orientation" true (Vector.orientation v_col = Vector.Column)

let test_dot () =
  let v1 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.of_array Vector.Row [|4.0; 5.0; 6.0|] in
  let result = Vector.dot v1 v2 in
  Alcotest.(check (float 0.0)) "dot product" 32.0 result

let test_dot_different_length () =
  let v1 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.of_array Vector.Row [|4.0; 5.0|] in
  Alcotest.check_raises "different length"
    (Invalid_argument "dot: vectors must have the same length")
    (fun () -> ignore (Vector.dot v1 v2))

let test_add () =
  let v1 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.of_array Vector.Row [|4.0; 5.0; 6.0|] in
  let v3 = Vector.add v1 v2 in
  Alcotest.(check (float 0.0)) "element 0" 5.0 (Vector.get v3 0);
  Alcotest.(check (float 0.0)) "element 1" 7.0 (Vector.get v3 1);
  Alcotest.(check (float 0.0)) "element 2" 9.0 (Vector.get v3 2)

let test_add_different_orientation () =
  let v1 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.of_array Vector.Column [|4.0; 5.0; 6.0|] in
  Alcotest.check_raises "different orientation"
    (Invalid_argument "add: vectors must have the same orientation")
    (fun () -> ignore (Vector.add v1 v2))

let test_add_different_length () =
  let v1 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.of_array Vector.Row [|4.0; 5.0|] in
  Alcotest.check_raises "different length"
    (Invalid_argument "add: vectors must have the same length")
    (fun () -> ignore (Vector.add v1 v2))

let test_sub () =
  let v1 = Vector.of_array Vector.Row [|4.0; 5.0; 6.0|] in
  let v2 = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v3 = Vector.sub v1 v2 in
  Alcotest.(check (float 0.0)) "element 0" 3.0 (Vector.get v3 0);
  Alcotest.(check (float 0.0)) "element 1" 3.0 (Vector.get v3 1);
  Alcotest.(check (float 0.0)) "element 2" 3.0 (Vector.get v3 2)

let test_sub_different_orientation () =
  let v1 = Vector.of_array Vector.Row [|4.0; 5.0; 6.0|] in
  let v2 = Vector.of_array Vector.Column [|1.0; 2.0; 3.0|] in
  Alcotest.check_raises "different orientation"
    (Invalid_argument "sub: vectors must have the same orientation")
    (fun () -> ignore (Vector.sub v1 v2))

let test_sub_different_length () =
  let v1 = Vector.of_array Vector.Row [|4.0; 5.0; 6.0|] in
  let v2 = Vector.of_array Vector.Row [|1.0; 2.0|] in
  Alcotest.check_raises "different length"
    (Invalid_argument "sub: vectors must have the same length")
    (fun () -> ignore (Vector.sub v1 v2))

let test_scale () =
  let v = Vector.of_array Vector.Row [|1.0; 2.0; 3.0|] in
  let v2 = Vector.scale 2.0 v in
  Alcotest.(check (float 0.0)) "element 0" 2.0 (Vector.get v2 0);
  Alcotest.(check (float 0.0)) "element 1" 4.0 (Vector.get v2 1);
  Alcotest.(check (float 0.0)) "element 2" 6.0 (Vector.get v2 2)

let () =
  let open Alcotest in
  run "Vector" [
    "basic", [
      test_case "create row" `Quick test_create_row;
      test_case "create column" `Quick test_create_column;
      test_case "get/set" `Quick test_get_set;
      test_case "get/set column" `Quick test_get_set_column;
      test_case "copy" `Quick test_copy;
      test_case "of_array/to_array" `Quick test_of_array_to_array;
    ];
    "transformations", [
      test_case "map" `Quick test_map;
      test_case "mapi" `Quick test_mapi;
      test_case "fold" `Quick test_fold;
    ];
    "comparison", [
      test_case "equal" `Quick test_equal;
    ];
    "conversion", [
      test_case "to_matrix" `Quick test_to_matrix;
      test_case "of_matrix" `Quick test_of_matrix;
      test_case "of_matrix invalid row" `Quick test_of_matrix_invalid_row;
      test_case "of_matrix invalid column" `Quick test_of_matrix_invalid_column;
      test_case "orientation" `Quick test_orientation;
    ];
    "operations", [
      test_case "dot" `Quick test_dot;
      test_case "dot different length" `Quick test_dot_different_length;
      test_case "add" `Quick test_add;
      test_case "add different orientation" `Quick test_add_different_orientation;
      test_case "add different length" `Quick test_add_different_length;
      test_case "sub" `Quick test_sub;
      test_case "sub different orientation" `Quick test_sub_different_orientation;
      test_case "sub different length" `Quick test_sub_different_length;
      test_case "scale" `Quick test_scale;
    ];
  ]

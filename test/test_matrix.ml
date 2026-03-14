open Linear_algebra

let test_create () =
  let m = Matrix.create 3 4 0.0 in
  Alcotest.(check int) "rows" 3 (Matrix.rows m);
  Alcotest.(check int) "cols" 4 (Matrix.cols m);
  Alcotest.(check (float 0.0)) "init value" 0.0 (Matrix.get m 0 0)

let test_get_set () =
  let m = Matrix.create 2 2 0.0 in
  Matrix.set m 0 1 5.0;
  Alcotest.(check (float 0.0)) "get after set" 5.0 (Matrix.get m 0 1)

let test_copy () =
  let m1 = Matrix.create 2 2 1.0 in
  Matrix.set m1 0 0 42.0;
  let m2 = Matrix.copy m1 in
  Matrix.set m1 0 0 0.0;
  Alcotest.(check (float 0.0)) "copy is independent" 42.0 (Matrix.get m2 0 0)

let test_of_array () =
  let arr = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let m = Matrix.of_array arr in
  Alcotest.(check (float 0.0)) "of_array (0,0)" 1.0 (Matrix.get m 0 0);
  Alcotest.(check (float 0.0)) "of_array (1,1)" 4.0 (Matrix.get m 1 1)

let test_map () =
  let m = Matrix.of_array [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let m2 = Matrix.map (fun x -> x *. 2.0) m in
  Alcotest.(check (float 0.0)) "map doubles" 2.0 (Matrix.get m2 0 0);
  Alcotest.(check (float 0.0)) "map doubles" 8.0 (Matrix.get m2 1 1)

let test_mapi () =
  let m = Matrix.create 2 2 0.0 in
  let m2 = Matrix.mapi (fun i j _ -> float_of_int (i + j)) m in
  Alcotest.(check (float 0.0)) "mapi (0,0)" 0.0 (Matrix.get m2 0 0);
  Alcotest.(check (float 0.0)) "mapi (1,1)" 2.0 (Matrix.get m2 1 1)

let test_fold () =
  let m = Matrix.of_array [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let sum = Matrix.fold ( +. ) 0.0 m in
  Alcotest.(check (float 0.0)) "fold sum" 10.0 sum

let test_equal () =
  let m1 = Matrix.of_array [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let m2 = Matrix.of_array [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let m3 = Matrix.of_array [| [| 1.0; 2.0 |]; [| 3.0; 5.0 |] |] in
  Alcotest.(check bool) "equal matrices" true (Matrix.equal m1 m2);
  Alcotest.(check bool) "not equal matrices" false (Matrix.equal m1 m3)

let () =
  Alcotest.run "Matrix"
    [
      ( "basic",
        [
          Alcotest.test_case "create" `Quick test_create;
          Alcotest.test_case "get_set" `Quick test_get_set;
          Alcotest.test_case "copy" `Quick test_copy;
          Alcotest.test_case "of_array" `Quick test_of_array;
        ] );
      ( "operations",
        [
          Alcotest.test_case "map" `Quick test_map;
          Alcotest.test_case "mapi" `Quick test_mapi;
          Alcotest.test_case "fold" `Quick test_fold;
          Alcotest.test_case "equal" `Quick test_equal;
        ] );
    ]

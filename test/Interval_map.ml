open Interval_map ;;

module Test =
struct

  module Test_interval_map (M : Interval_map_type (Int_ord).T) =
    struct
      let intervals_of m =
        M.fold (fun key _ keys -> key :: keys) m []

      let add x = M.add (=) x

      let test_empty () =
        assert (not (M.mem 0 M.empty)) ;
        assert (intervals_of M.empty = [])

      let flip f x y = f y x
      let test_mem () =
        let m = add (0, 3) true (add (2, 4) false M.empty) in
        assert (List.for_all (flip M.mem m) [0;1;2;3;4])

      let rec subset xs ys =
        match xs with
          | [] -> true
          | x :: xs -> List.mem x ys && subset xs ys
      let seteq xs ys = subset xs ys && subset ys xs

      let test_add () =
        let m = add (0, 3) true (add (2, 4) false M.empty) in
        assert (seteq (intervals_of m) [(0, 3); (4, 4)]) ;
        assert (2 = List.length (intervals_of m))

      let test_find () =
        let m = add (0, 3) true (add (2, 4) false M.empty) in
        assert (M.find 0 m) ;
        assert (M.find 1 m) ;
        assert (M.find 2 m) ;
        assert (M.find 3 m) ;
        assert (not (M.find 4 m))

      let (|>) x f = f x
      let test_update () =
        let m = M.empty
          |> add (0, 6) 0
          |> add (4, 9) 1
          |> add (3, 4) 2
          |> add (6, 7) 3 in
        let m = M.update (=) (0, 10) ((+) 1) 4 m in
        assert (M.find 0 m = 1) ;
        assert (M.find 1 m = 1) ;
        assert (M.find 2 m = 1) ;
        assert (M.find 3 m = 3) ;
        assert (M.find 4 m = 3) ;
        assert (M.find 5 m = 2) ;
        assert (M.find 6 m = 4) ;
        assert (M.find 7 m = 4) ;
        assert (M.find 8 m = 2) ;
        assert (M.find 9 m = 2) ;
        assert (M.find 10 m = 5)

      let test_values () =
        let m = M.empty
          |> add (0, 6) 0
          |> add (4, 9) 1
          |> add (3, 4) 2
          |> add (6, 7) 3 in
        assert (seteq (M.values (0, 9) m) [0;1;2;3]) ;
        assert (seteq (M.values (0, 5) m) [0;1;2]) ;
        assert (seteq (M.values (7, 9) m) [1;3])

      let test_equal () =
        let m = M.empty
          |> add (0, 6) 0
          |> add (4, 9) 1
          |> add (3, 4) 2
          |> add (6, 7) 3 in
        let n = M.empty
          |> add (0, 0) 0
          |> add (1, 1) 0
          |> add (2, 2) 0
          |> add (3, 3) 2
          |> add (4, 4) 2
          |> add (5, 5) 1
          |> add (6, 6) 3
          |> add (7, 7) 3
          |> add (8, 8) 1
          |> add (9, 9) 1 in
        assert (M.equal (=) m n) ;
        assert (not (M.equal (=) m (add (1, 5) 42 n)))

      let test () =
        test_empty () ;
        test_mem () ;
        test_add () ;
        test_find () ;
        test_update () ;
        test_values () ;
        test_equal ()
    end
  module Test_imap = Test_interval_map (Interval_map (Int_ord))
  module Test_amap = Test_interval_map (Array_interval_map (Int_ord))

  let test () =
    Test_imap.test () ;
    Test_amap.test ()

end ;;

Test.test () ;;

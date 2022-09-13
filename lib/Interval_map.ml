module type Ordered_type =
sig
  type t
  val compare : t -> t -> int
end

module Option_ordered (I : Ordered_type) =
  struct
    type t = I.t option
    let compare x y =
      match (x, y) with
        | (None, None) -> 0
        | (None, Some _) -> -1
        | (Some _, None) -> 1
        | (Some x, Some y) -> I.compare x y
  end

module type Ordered_incrementable_type =
sig
  include Ordered_type
  val successor : t -> t
  val predecessor : t -> t
end

let lex_compare cmp0 cmp1 (x0, x1) (y0, y1) =
  let d0 = cmp0 x0 y0 in
  if d0 <> 0 then d0
  else cmp1 x1 y1

module Lexicographic_incrementable_ord :
  functor (P : Ordered_incrementable_type) ->
    functor (Q : Ordered_type) ->
      Ordered_incrementable_type with type t = P.t * Q.t =
  functor (P : Ordered_incrementable_type) ->
    functor (Q : Ordered_type) ->
        struct
          type t = P.t * Q.t
          let compare (x0, x1) (y0, y1) =
            lex_compare Q.compare P.compare (x1, x0) (y1, y0)
          let successor (x, y) = (P.successor x, y)
          let predecessor (x, y) = (P.predecessor x, y)
        end

module Min_max (O : Ordered_type) =
struct
  let min x y =
    let diff = O.compare x y in
    if diff < 0 then x
    else y
  let max x y =
    let diff = O.compare x y in
    if diff > 0 then x
    else y
end

type 'a interval_mode =
  | Query of 'a
  | Overlap of 'a * 'a
  | Entry of 'a * 'a

module Interval_ord (I : Ordered_type)
  : Ordered_type with type t = I.t interval_mode =
struct
  type t = I.t interval_mode

  let (-) x y = I.compare x y
  let (<=) x y = I.compare x y <= 0
  let (<) x y = I.compare x y < 0

  let rec compare x y =
    match x, y with
      | Query x, Query y -> x - y
      | Query x, (Overlap (y0, y1) | Entry (y0, y1)) ->
          assert (y0 <= y1) ;
          if x < y0 then x - y0
          else if y1 < x then x - y1
          else 0
      | Overlap (x0, x1), (Overlap (y0, y1) | Entry (y0, y1)) ->
          assert (x0 <= x1) ;
          assert (y0 <= y1) ;
          if x1 < y0 then x1 - y0
          else if y1 < x0 then x0 - y1
          else 0
      | Entry (x0, x1), Entry (y0, y1) ->
          if x1 < y0 then x1 - y0
          else if y1 < x0 then x0 - y1
          else if x0 = y0 && x1 = y1 then 0
          else failwith "Interval_ord.compare: Comparing two overlapping entries!"
      | x, y -> compare y x
end


module Intervals (I : Ordered_incrementable_type) =
struct
  let (<=) x y = I.compare x y <= 0
  let (<) x y =
    (
    I.compare x y < 0)
  let (>) x y = I.compare x y > 0

  let min x y = if x <= y then x else y
  let max x y = if x <= y then y else x

  type diff_result =
    | Nothing
    | One_interval of I.t * I.t
    | Two_intervals of (I.t * I.t) * (I.t * I.t)

  let interval_diff (x0, x1) (y0, y1) =
    assert (x0 <= x1) ;
    assert (y0 <= y1) ;
    if x0 < y0 then
      (if x1 < y0 then
        Nothing
      else (* y0 <= x1 *)
        (if x1 <= y1 then
          (assert (x0 < y0) ;
          One_interval (x0, (I.predecessor y0)))
        else (* y1 < x1 *)
          Two_intervals ((x0, I.predecessor y0),
                         (I.successor y1, x1))))
    else (* y0 <= x0 *)
      if y1 < x0 then
        Nothing
      else (* x0 <= y1 *)
        if x1 <= y1 then
          Nothing
        else (* y1 < x1 *)
          (assert (I.successor y1 <= x1) ;
          One_interval (I.successor y1, x1))
  let interval_inter (x0, x1) (y0, y1) =
    assert (x0 <= x1) ;
    assert (y0 <= y1) ;
    if x1 < y0 then None
    else if y1 < x0 then None
    else Some (max x0 y0, min x1 y1)
end

module Interval_map_type =
  functor (I : Ordered_incrementable_type) ->
    struct
      module type T =
        sig
          type key = I.t * I.t
          type 'a t
          val empty : 'a t
          val mem : I.t -> 'b t -> bool
          val find : I.t -> 'c t -> 'c
          val add :
            ('a -> 'a -> bool) ->
            I.t * I.t -> 'a -> 'a t -> 'a t
          val update :
            ('a -> 'a -> bool) ->
            I.t * I.t -> ('a -> 'a) -> 'a -> 'a t -> 'a t
          val values : I.t * I.t -> 'a t -> 'a list
          val fold :
            (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        end
    end

let array_for_all2 f xs ys =
  if Array.length xs <> Array.length ys then
    false
  else
    let res = ref true in
    for i = 0 to Array.length xs - 1 do
      res := !res && f xs.(i) ys.(i)
    done ;
    !res

module Array_interval_map (I : Ordered_incrementable_type)
 (*: Interval_map_type(I).T*) =
    struct
      module I_min_max = Min_max (I)
      type key = I.t * I.t
      (*  key array * value array   The key array has twice the size of the value array. *)
      type 'a t = I.t array * 'a array
      let empty = ([||], [||])

      let inv_length (keys, values) =
        Array.length keys = 2 * Array.length values
      let inv_sorted (keys, _) =
        let a = Array.copy keys in
        Array.sort I.compare a ;
        array_for_all2 (fun x y -> 0 = I.compare x y) keys a

      let mem x (keys, values) =
        assert (inv_length (keys, values)) ;
        assert (inv_sorted (keys, values)) ;
        let rec mem_rec l r =
          if l = r then
            I.compare keys.(2 * l) x <= 0 &&
            I.compare x keys.(2 * l + 1) <= 0
          else
            let m = (l + r) / 2 in
            if I.compare x keys.(2 * m) < 0 then
              mem_rec l (m - 1)
            else if I.compare x keys.(2 * m + 1) > 0 then
              mem_rec (m + 1) r
            else mem_rec m m in
        Array.length values > 0 &&
          mem_rec 0 (Array.length values - 1)

      let find x (keys, values) =
        assert (inv_length (keys, values)) ;
        assert (inv_sorted (keys, values)) ;
        let rec find_rec l r =
          if l = r then
            if I.compare keys.(2 * l) x <= 0 &&
               I.compare x keys.(2 * l + 1) <= 0 then
              values.(l)
            else
              raise Not_found
          else
            let m = (l + r) / 2 in
            if I.compare x keys.(2 * m) < 0 then
              find_rec l (m - 1)
            else if I.compare x keys.(2 * m + 1) > 0 then
              find_rec (m + 1) r
            else find_rec m m in
        if Array.length values <= 0 then
          raise Not_found
        else
          find_rec 0 (Array.length values - 1)

      let find_index x (keys, values) =
        assert (inv_length (keys, values)) ;
        assert (inv_sorted (keys, values)) ;
        let rec find_rec l r =
          if l = r then l
          else
            let m = (l + r) / 2 in
            if I.compare x keys.(2 * m + 1) <= 0 then
              find_rec l m
            else
              find_rec (m + 1) r in
        if Array.length values <= 0 then
          0
        else
          find_rec 0 (Array.length values - 1)

      module Ints = Intervals (I)

      let values (l, r) (keys, values) =
        assert (inv_length (keys, values)) ;
        assert (inv_sorted (keys, values)) ;
        let l0 = find_index l (keys, values) in
        let i = ref l0 in
        let dun = ref false in
        let result = ref [] in
        while not !dun && !i < Array.length values do
          (match Ints.interval_inter (keys.(2 * !i), keys.(2 * !i + 1))
                  (l, r) with
            | Some _ -> result := values.(!i) :: !result
            | None ->
                if I.compare r keys.(2 * !i + 1) <= 0 then
                begin
                  dun := true ;
                end) ;
          incr i
        done ;
        List.rev !result
      let compact eq (keys, values) =
        assert (inv_length (keys, values)) ;
        assert (inv_sorted (keys, values)) ;
        let rec compact_rec v l r i acc =
          if i < Array.length values && eq v values.(i) then
            compact_rec v l (keys.(2 * i + 1)) (i + 1) acc
          else
            let acc = ((l, r), v) :: acc in
            if i >= Array.length values then
              acc
            else
              compact_rec values.(i) keys.(2 * i)
                keys.(2 * i + 1) (i + 1) acc in
        if Array.length values > 0 then
          let xs = compact_rec values.(0) keys.(0) keys.(1) 1 [] in
          let new_keys = Array.make (2 * List.length xs) keys.(0) in
          let new_values = Array.make (List.length xs) values.(0) in
          let i = ref 0 in
          List.iter (fun ((l, r), v) ->
              new_keys.(2 * !i) <- l ;
              new_keys.(2 * !i + 1) <- r ;
              new_values.(!i) <- v ;
              incr i) (List.rev xs) ;
          (new_keys, new_values)
        else
          ([||], [||])

      let update eq (l, r) f x (keys, values) =
        assert (inv_length (keys, values)) ;
        assert (inv_sorted (keys, values)) ;
        if Array.length values = 0 then
          ([| l; r |], [| f x |])
        else
        let rec update_left i acc =
          if i >= Array.length values then
            ((l, r), f x) :: acc
          (* [ . ] .. L *)
          else if I.compare keys.(2 * i + 1) l < 0 then
            update_left (i + 1) (((keys.(2 * i), keys.(2 * i + 1)),
                                  values.(i)) :: acc)
          (* [ .. L . ] *)
          else if I.compare keys.(2 * i) l < 0 then
            update_middle l i acc
            (*update_middle l i (((keys.(2 * i), I.predecessor l),
                            values.(i)) :: acc)*)
          else (* L . [ . ] *)
            update_middle l i acc
        and update_middle l i acc =
          if i >= Array.length values then
            ((l, r), f x) :: acc
          else (
            (* L . ] *)
            assert (I.compare l keys.(2 * i + 1) <= 0) ;
            (* L . R *)
            assert (I.compare l r <= 0) ;
            (* L .. [ *)
            if I.compare l keys.(2 * i) < 0 then
              let r' = I_min_max.min r (I.predecessor keys.(2 * i)) in
              (* L . R .. [ . ] *)
              if I.compare r keys.(2 * i) < 0 then
                update_right i (((l, r'), f x) :: acc)
              (* L .. [ . R *)
              else
                let acc = (((l, r'), f x) :: acc) in
                (* L .. [ . ] .. R *)
                if I.compare keys.(2 * i + 1) r < 0 then
                  update_middle (I.successor keys.(2 * i + 1)) (i + 1)
                    (((keys.(2 * i), keys.(2 * i + 1)), f values.(i))
                      :: acc)
                (* L .. [ . R .. ] *)
                else if I.compare r keys.(2 * i + 1) < 0 then
                  update_right (i + 1) (
                    ((I.successor r, keys.(2 * i + 1)), values.(i)) ::
                    ((keys.(2 * i), r), f values.(i)) ::
                    acc)
                (* L .. [ . R] *)
                else
                  update_right (i + 1) (
                      ((keys.(2 * i), keys.(2 * i + 1)), f values.(i)) ::
                      acc)
            (* [ . L *)
            else
              let acc =
                if I.compare keys.(2 * i) l < 0 then
                  ((keys.(2 * i), I.predecessor l), values.(i)) :: acc
                else acc in
              (* [ . L . ] .. R *)
              if I.compare keys.(2 * i + 1) r < 0 then
                update_middle (I.successor keys.(2 * i + 1))
                  (i + 1) (((l, keys.(2 * i + 1)), f values.(i)) :: acc)
              (* [ . L . R .. ] *)
              else if I.compare r keys.(2 * i + 1) < 0 then
                update_right (i + 1) (
                    ((I.successor r, keys.(2 * i + 1)), values.(i)) ::
                    ((l, r), f values.(i)) ::
                    acc)
              (* [ . L . R] *)
              else
               update_right (i + 1) (((l, r), f values.(i)) :: acc))
        and update_right i acc =
          if i >= Array.length values then acc
          else
            update_right (i + 1)
             (((keys.(2 * i), keys.(2 * i + 1)), values.(i)) :: acc) in
        let value_list = List.rev (update_left 0 []) in
        let new_keys = Array.make (2 * List.length value_list) keys.(0) in
        let new_values = Array.make (List.length value_list) values.(0) in
        let i = ref 0 in
        List.iter (fun ((l, r), v) ->
            new_keys.(2 * !i) <- l ;
            new_keys.(2 * !i + 1) <- r ;
            new_values.(!i) <- v ;
            incr i) value_list ;
        compact eq (new_keys, new_values)

      let const c _ = c
      let add eq (l, r) x m =
        assert (inv_length m) ;
        assert (inv_sorted m) ;
        update eq (l, r) (const x) x m

      let fold f (keys, values) acc =
        assert (inv_length (keys, values)) ;
        assert (inv_sorted (keys, values)) ;
        let state = ref acc in
        for i = 0 to Array.length values - 1 do
          state := f (keys.(2 * i), keys.(2 * i + 1)) values.(i) !state
        done ;
        !state
      let equal eq (keys0, values0) (keys1, values1) =
        if Array.length values0 <> Array.length values1 ||
          Array.length keys0 <> Array.length keys1 then
          false
        else
          let result = ref true in
          for i = 0 to Array.length values0 - 1 do
            if !result && not (eq values0.(i) values1.(i)) ||
               0 <> I.compare keys0.(2 * i) keys1.(2 * i) ||
               0 <> I.compare keys0.(2 * i + 1) keys1.(2 * i + 1) then
            begin
              result := false
            end
          done ;
          !result
    end

module Interval_map =
  functor (I : Ordered_incrementable_type) ->
      struct
        module I2 = Lexicographic_incrementable_ord (I) (I)
        module In : Ordered_type with type t = I.t interval_mode =
          Interval_ord (I)
        module M : Map.S with type key = In.t
          and type 'a t = 'a Map.Make(In).t =
          Map.Make (In)
        type key = I.t * I.t
        type 'a t = ((I.t * I.t) * 'a) M.t

        let empty = M.empty
        let mem i m =
          M.mem (Query i) m
        let find i m =
          snd (M.find (Query i) m)

        include Intervals (I)

        module I_min_max = Min_max (I)

        let rec add eq_v (l, r) v m =
          if l > r then
              failwith ("Interval_map.add: l > r") else
          (* if there are overlapping intervals *)
          if M.mem (Overlap (l, r)) m then
            (* pick one of the overlapping intervals *)
            let ((l0, r0), v0) = M.find (Overlap (l, r)) m in
            assert (l0 <= r0) ;
            let m = M.remove (Entry (l0, r0)) m in
            if eq_v v v0 then
              (* unite the overlapping intervals *)
              let l1 = I_min_max.min l l0 in
              let r1 = I_min_max.max r r0 in
              (* remove the old overlapping interval and recurse *)
              add eq_v (l1, r1) v m
            else
              let diff = interval_diff (l0, r0) (l, r) in
              match diff with
                | Nothing ->
                    (* the old interval is covered by (l, r) *)
                    add eq_v (l, r) v m
                | One_interval (l2, r2) ->
                    assert (l2 <= r2) ;
                    (* add the "new old" interval and recurse *)
                    add eq_v (l, r) v (
                      M.add (Entry (l2, r2)) ((l2, r2), v0) m)
                | Two_intervals ((l2, r2), (l3, r3)) ->
                    (* add the "new old" intervals and recurse *)
                    assert (l2 <= r2) ;
                    assert (l3 <= r3) ;
                     (
                      M.add (Entry (l2, r2)) ((l2, r2), v0) (
                        M.add (Entry (l3, r3)) ((l3, r3), v0) (add eq_v (l, r) v m)))
          else
            let rec join_intervals (l, r) m =
              (* create a slightly larger interval *)
              let (ll, rr) = (I.predecessor l, I.successor r) in
              (* if there are adjacent intervals *)
              if M.mem (Overlap (ll, rr)) m then
                let ((l0, r0), v0) = M.find (Overlap (ll, rr)) m in
                (* if the values are equal *)
                if eq_v v0 v then
                  (* join the intervals *)
                  let l1 = I_min_max.min l l0 in
                  let r1 = I_min_max.max r r0 in
                  (* remove the smaller interval, insert the larger one *)
                  join_intervals (l1, r1) (
                      M.remove (Entry (l0, r0)) m)
                else (* not equal values *)
                  (* remove the adjacent interval to see if there are others but reinsert it afterwards *)
                  (
                    assert (l0 <= r0) ;
                  M.add (Entry (l0, r0)) ((l0, r0), v0) (
                    join_intervals (l, r)
                      (M.remove (Entry (l0, r0)) m))
                  )
              else (* no more adjacent intervals => simply insert the entry *)
                (assert (l <= r) ;
                M.add (Entry (l, r)) ((l, r), v) m) in
            join_intervals (l, r) m

        let fold f xs acc = List.fold_left (fun x acc -> f acc x) acc xs
        let update eq_v (l, r) f v m =
          let rec collect_intervals m acc =
            if M.mem (Overlap (l, r)) m then
              let ((l0, r0), v0) = M.find (Overlap (l, r)) m in
              let interval = interval_inter (l0, r0) (l, r) in
              match interval with
                | Some interval ->
                    collect_intervals (M.remove (Entry (l0, r0)) m)
                      ((interval, v0) :: acc)
                | None ->
                    failwith ("Interval_map.update: Overlapping " ^
                              "interval doesn't overlap!")
            else
              ((l, r), v) :: acc in
          let intervals = collect_intervals m [] in
          fold (fun ((l, r), v) -> add eq_v (l, r) (f v)) intervals m

        let values (l, r) m =
          let rec collect_values m acc =
            if M.mem (Overlap (l, r)) m then
              let ((l0, r0), v0) = M.find (Overlap (l, r)) m in
              collect_values (M.remove (Entry (l0, r0)) m) (v0 :: acc)
            else
              acc in
          collect_values m []

        let fold f =
          M.fold (fun x (x', y) ->
              assert (
                match x with
                  | Entry (x0, x1) -> (x0, x1) = x'
                  | _ -> false) ;
              match x with
                | Entry (l, r) ->
                    f (l, r) y
                | _ -> failwith "Interval_map.fold: found entry without constructor Entry!")

        let equal eq_v p q =
          let keys_p =
            List.stable_sort I2.compare
              (fold (fun key _ keys -> key :: keys) p []) in
          let keys_q =
            List.stable_sort I2.compare
              (fold (fun key _ keys -> key :: keys) q []) in
          List.length keys_p = List.length keys_q &&
          List.for_all2 (fun (lp, rp) (lq, rq) ->
            0 = I.compare lp lq &&
            0 = I.compare rp rq &&
            let value_p = find lp p in
            let value_q = find lq q in
            eq_v value_p value_q) keys_p keys_q
      end

module type Interval_set_type =
sig
  type elt
  type t
  val empty : t
  val mem : elt -> t -> bool
  val add : elt * elt -> t -> t
  val fold : (elt * elt -> 'a -> 'a) -> t -> 'a -> 'a
  val equal : t -> t -> bool
end

module Interval_set_type =
  functor (I : Ordered_incrementable_type) ->
    struct
      module type T = Interval_set_type with type elt = I.t
    end

module Interval_set =
  functor (I : Ordered_incrementable_type) ->
    struct
      module M = Interval_map (I)
      type elt = I.t
      type t = unit M.t
      let empty = M.empty
      let mem = M.mem
      let add (l, r) s =
        assert (l <= r) ;
          M.add (fun () () -> true) (l, r) () s
      let fold f =
        M.fold (fun p () -> f p)
      let equal = M.equal (fun () () -> true)
    end

module Int_ord : Ordered_incrementable_type with type t = int =
struct
  type t = int
  let compare x y =
    x - y
  let successor x =
    if x < max_int then
      (
      x + 1)
    else x
  let predecessor x =
    if x > min_int then
      (
      x - 1)
    else x
end

module Char_ord : Ordered_incrementable_type with type t = char =
struct
  type t = char
  let compare x y =
    Char.code x - Char.code y
  let successor x =
    if x < Char.chr 255 then
      Char.chr (Char.code x + 1)
    else x
  let predecessor x =
    if x > Char.chr 0 then
      Char.chr (Char.code x - 1)
    else x
end

module Option_ord (O : Ordered_incrementable_type)
  : Ordered_incrementable_type with type t = O.t option =
struct
  include Option_ordered (O)
  let successor = function
    | Some x -> Some (O.successor x)
    | None -> None
  let predecessor = function
    | Some x -> Some (O.predecessor x)
    | None -> None
end

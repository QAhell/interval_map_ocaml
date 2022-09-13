(** This module contains the implementation of interval maps. *)

(** A type that can be ordered linearly. *)
module type Ordered_type =
sig
  type t
  val compare : t -> t -> int
end

(** An [Ordered_type] that can be incremented and decremented. *)
module type Ordered_incrementable_type =
  sig
    include Ordered_type

    (** Returns the incremented value. If the argument
        is the maximum value, the argument will be returned
        unchanged. *)
    val successor : t -> t

    (** Returns the decremented value. If the argument
        is the minimum value, the argument will be returned
        unchanged. *)
    val predecessor : t -> t
  end

(** Lexicographic order of pairs with increment and decrement
    operations. Only the first component is incremented or
    decremented. *)
module Lexicographic_incrementable_ord :
  functor (P : Ordered_incrementable_type) ->
    functor (Q : Ordered_type) ->
      Ordered_incrementable_type with type t = P.t * Q.t

(** Type type of a map module where intervals are the keys. *)
module Interval_map_type :
  functor (I : Ordered_incrementable_type) ->
    sig
      module type T =
        sig
          (** The type of the keys in the map. *)
          type key = I.t * I.t

          (** The type of the map itself. *)
          type 'a t

          (** An empty map. *)
          val empty : 'a t

          (** Returns [true] if the element is contained
            in a key of the map. *)
          val mem : I.t -> 'b t -> bool

          (** Returns the value that is mapped to the key. *)
          val find : I.t -> 'c t -> 'c

          (** Given an interval and a value, this function
            returns the updated map. The equality on the
            values must be provided. *)
          val add :
            ('a -> 'a -> bool) ->
            I.t * I.t -> 'a -> 'a t -> 'a t

          (** [update eq (l, r) f v m] updates the map [m]
             in the interval [(l, r)]. If there's already
             a value in the map, [f] is applied to it.
             Otherwise, [f v] is added to the map. *)
          val update :
            ('a -> 'a -> bool) ->
            I.t * I.t -> ('a -> 'a) -> 'a -> 'a t -> 'a t

          (** Returns a list of values for a given interval *)
          val values : I.t * I.t -> 'a t -> 'a list

          (** Iterates through all entries of the map. *)
          val fold :
            (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

          (** Equality of maps. *)
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        end
    end

(** A map module where intervals are the keys. *)
module Interval_map :
  functor (I : Ordered_incrementable_type) ->
    Interval_map_type (I).T with type key = I.t * I.t

(** An implementation of interval maps based on arrays.  *)
module Array_interval_map :
  functor (I : Ordered_incrementable_type) ->
    Interval_map_type (I).T with type key = I.t * I.t

(** Type of a module containing sets of intervals. *)
module type Interval_set_type =
sig
  (** The type of the elements of the set. *)
  type elt

  (** The type of the set. *)
  type t

  (** An empty set. *)
  val empty : t

  (** Returns [true] if the element is contained in an
     interval from the set. *)
  val mem : elt -> t -> bool

  (** Inserts a new interval into the set. *)
  val add : elt * elt -> t -> t

  (** Iterates through all entries of the set. *)
  val fold : (elt * elt -> 'a -> 'a) -> t -> 'a -> 'a

  (** Equality of sets. *)
  val equal : t -> t -> bool
end

(** Type of a module containing sets of intervals. *)
module Interval_set_type :
  functor (I : Ordered_incrementable_type) ->
    sig
      module type T = Interval_set_type with type elt = I.t
    end

(** A module containing sets of intervals.
  The implementation uses [Interval_map]. *)
module Interval_set :
  functor (I : Ordered_incrementable_type) ->
    Interval_set_type (I).T

(** int as an incrementable ordered type. *)
module Int_ord : Ordered_incrementable_type with type t = int

(** char as an incrementable ordered type. *)
module Char_ord : Ordered_incrementable_type with type t = char

(** option types as incrementable ordered types. *)
module Option_ord :
  functor (O : Ordered_incrementable_type) ->
    Ordered_incrementable_type with type t = O.t option

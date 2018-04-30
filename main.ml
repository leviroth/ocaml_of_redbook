module Monad = struct
  module type Base = sig
    type 'a t
    val return : 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module type S = sig
    include Base
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  end

  module Make (B : Base) : S with type 'a t := 'a B.t = struct
    include B

    let (>>|) m f = m >>= fun x -> return (f x)

    let map2 m1 m2 ~f =
      m1 >>= fun a ->
      m2 >>| fun b ->
      f a b
  end
end

module type STATE = sig
  type state
  include Monad.S with type 'a t = state -> state * 'a
  val get : state t
  val put : state -> unit t
  val run : 'a t -> init:state -> state * 'a
end

module State (S : sig type t end)
  : STATE with type state = S.t = struct
  type state = S.t
  module Monad_base : Monad.Base with type 'a t = state -> state * 'a = struct
    type 'a t = state -> state * 'a
    let return v s = (s, v)
    let (>>=) m k s = let s', a = m s in k a s'
  end
  include Monad_base
  include Monad.Make(Monad_base)
  let get s = (s, s)
  let put s _ = (s, ())
  let run m ~init = m init
end

module RNG : sig
  include STATE with type state = int
  val of_int : int -> state
  val next_int : int t
  val next_float : float t
  val int_float : (int * float) t
end = struct
  include State (struct type t = int end)
  let of_int n = n
  let next_int s =
    let new_seed = (s * 0x5deece66d + 0xB) land 0xfffffffff in
    let n = new_seed lsr 16 in
    new_seed, n
  let next_float =
    next_int >>|
    float_of_int
  let int_float = map2 next_int next_float ~f:(fun a b -> a, b)
end

let%test _ =
  let rng = RNG.of_int 0 in
  let rng2, output1 = RNG.next_int rng in
  output1 = 0
  && snd (RNG.next_int rng2) = 37933

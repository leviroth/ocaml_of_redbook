module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
end

module type STATE = sig
  type state
  include MONAD with type 'a t = state -> state * 'a
  val get : state t
  val put : state -> unit t
  val run : 'a t -> init:state -> state * 'a
end

module State (S : sig type t end)
  : STATE with type state = S.t = struct
  type state = S.t
  type 'a t = state -> state * 'a
  let return v s = (s, v)
  let (>>=) m k s = let s', a = m s in k a s'
  let (>>|) m k s = let s', a = m s in (s', k a)
  let get s = (s, s)
  let put s _ = (s, ())
  let run m ~init = m init
  let map2 m1 m2 ~f s =
    let s', a = m1 s in
    let s'', b = m2 s' in
    s'', f a b
  let map2 m1 m2 ~f =
    m1 >>= fun a ->
    m2 >>| fun b ->
    f a b
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

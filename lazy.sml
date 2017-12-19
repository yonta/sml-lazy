signature LAZY =
sig
  type 'a susp
  val $ : (unit -> 'a) -> 'a susp
  val force : 'a susp -> 'a
end

structure Lazy :> LAZY =
struct
  datatype 'a lazy = Done of 'a | Lazy of unit -> 'a

  type 'a susp = 'a lazy ref

  fun $ cls = ref (Lazy cls)

  fun force (ref (Done x)) = x
    | force (s as ref (Lazy cls)) =
  let
    val x = cls ()
    val () = s := Done x
  in
    x
  end
end

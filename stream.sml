signature STREAM =
sig
  datatype 'a streamcell = Nil | Cons of 'a * 'a streamcell Lazy.susp
  type 'a stream = 'a streamcell Lazy.susp
  (* Oooops, limitation of SML Def.
  datatype 'a streamcell = Cons of 'a * 'a stream | Nil
  withtype 'a stream = 'a streamcell susp
   *)

  val null    : 'a stream -> bool
  val ++      : 'a stream * 'a stream -> 'a stream
  val take    : int * 'a stream -> 'a stream
  val drop    : int * 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
end

local
  open Lazy
in
structure Stream : STREAM =
struct
  datatype 'a streamcell = Nil | Cons of 'a * 'a stream
  withtype 'a stream = 'a streamcell susp

  (* streamをopaqueにした場合はnull/hd/tlを提供する *)
  (*
  fun hdc sc = case sc of Cons (a, _) => a | Nil => raise Empty
  fun tlc sc = case sc of Cons (_, s) => force s | Nil => raise Empty
  fun hd s = case force s of Cons (a, _) => a | Nil => raise Empty
  fun tl s = $ (fn () => case force s of Cons (_, s) => s | Nil => raise Empty)
   *)

  fun null s = case force s of Nil => true | Cons _ => false

  infix ++
  fun s1 ++ s2 =
    $ (fn () =>
          case force s1 of
              Cons (x, s) => Cons (x, s ++ s2)
            | Nil => force s2)

  fun take (0, _) = $ (fn () => Nil)
    | take (n, s) =
      $ (fn () =>
            case force s of
                Nil => Nil
              | Cons (x, s') => Cons (x, take (n-1, s')))

  (* int * 'a stream -> 'a stream *)
  fun strictDrop (0, s) = s
    | strictDrop (n, s) =
      case force s of
          Nil => $ (fn () => Nil)
        | Cons (_, s') => strictDrop (n-1, s')

  fun drop (n, s) = $ (fn () => force (strictDrop (n, s)))

  fun reverse s =
    let
      fun reverse' (s, r) =
        case force s of
            Nil => r
          | Cons (x, s') => reverse' (s', $ (fn () => Cons (x, r)))
    in
      reverse' (s, $ (fn () => Nil))
    end

end
end  (* local open Lazy *)

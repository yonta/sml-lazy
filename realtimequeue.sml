signature QUEUE =
sig
  type elem
  type queue

  val empty   : queue
  val isEmpty : queue -> bool

  val snoc    : queue * elem -> queue
  val head    : queue -> elem
  val tail    : queue -> queue
end

(*
 * value restrictionを避けるために、RealTimeQueueモジュールは
 * structureではなくfunctorとしてtypeを受け取る。
 * これにより、Queueの実体の生成時にQueueは一意の型を要素として持つ。
 *)
functor RealTimeQueue (type t) :> QUEUE where type elem = t =
struct
  type elem = t

  type queue =
       {
         front : elem Stream.stream,
         rear : elem list,
         accum : elem Stream.stream
       }

  val empty =
      {
        front = Lazy.$ (fn () => Stream.Nil),
        rear = nil,
        accum = Lazy.$ (fn () => Stream.Nil)
      }

  fun isEmpty {front, ...} = Stream.null front

  (* fun makeQueue f r a = {front = f, rear = r, accum = a} *)

  (* 'a queue -> 'a stream *)
  fun rotate {front, rear, accum} =
    case (Lazy.force front, rear) of
        (Stream.Nil, [y]) => Lazy.$ (fn () => Stream.Cons (y, accum))
      | (Stream.Cons (x, xs), y :: ys) =>
        Lazy.$ (fn () =>
              Stream.Cons (x, rotate {front = xs, rear = ys,
                                      accum =
                                      Lazy.$ (fn () => Stream.Cons (y, accum))}))
      | _ => raise Fail "RealTimeQueue.rotate: not supported pattern"

  fun exec (q as {front, rear, accum}) =
    case Lazy.force accum of
        Stream.Cons (_, s) => q # {accum = s}
      | Stream.Nil => let val front' = rotate q
                      in {front = front', rear = nil, accum = front'} end

  fun snoc (q as {rear, ...}, x) = exec (q # {rear = x :: rear})

  fun head {front, ...} =
    case Lazy.force front of
        Stream.Cons (x, _) => x
      | Stream.Nil => raise Empty

  fun tail (q as {front, ...}) =
    case Lazy.force front of
        Stream.Cons (x, s) => exec (q # {front = s})
      | Stream.Nil => raise Empty
end

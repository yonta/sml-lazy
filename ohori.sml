fun makeMemoFun f =
  let
    exception NotThere
    val memo = ref (fn x => (raise NotThere))
  in
    fn x => !memo x
            handle NotThere =>
                   let
                     val v = f x
                     val oldMemo = !memo
                     val () = memo := (fn y => if x = y then v else oldMemo y)
                   in
                     v
                   end
  end

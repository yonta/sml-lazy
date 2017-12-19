local
  open Lazy
in
val x = $ (fn () => (print "Evaluated!\n"; 0))
val y = x
val z1 = force x
val z2 = force y
end

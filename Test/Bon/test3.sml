let f (x:int) : int =
  let (y:int) = x+3 in
  let (x:int) = 5 in
  x+y


let main() : int = f(9)

(* main() s'évalue vers 17 *)

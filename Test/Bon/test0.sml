let f (r:int) : int = r

let main () : unit =
 let (val:int) = 10 in f(val)+val ; print_int(val*3)
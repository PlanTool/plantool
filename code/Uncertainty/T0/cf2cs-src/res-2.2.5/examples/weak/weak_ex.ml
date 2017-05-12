(* Demonstrates the correct behaviour of resizable weak arrays. *)

(* module W = Res.Weak *)
module Array = Weak  (* allows more convenient array access *)

class foo = object end

type alpha = Base of int | Case of string;;

let max = 300;;

let ra = Array.create (max+1) (* (Base 0) *)

let l1 = [1;2]
let val1 = ref(Base 1)
let val2 = ref(Base 2)

let _ =
  begin
    ra.(0) <- Some !val1;
    ra.(1) <- Some !val2;
    ra.(2) <- Some !val2;
(*     for i = 0 to max/2 do *)
(*     done; *)
    for i = 2 to max do
      ra.(i) <- Some (Base (i*2))
    done;
(*     ra.(0) <- Some (Base 1); *)
(*     ra.(1) <- Some (Base 2); *)
(*     ra.(2) <- Some (Base 3); *)

(*     W.add_one ra (Some (Base 1)); *)
(*     W.add_one ra (Some (Base 2)); *)
(*     W.add_one ra (Some (Base 3)); *)

(*     W.add_one ra (Some (new foo)); *)
(*     W.add_one ra (Some (new foo)); *)
(*     W.add_one ra (Some (new foo)); *)
    for i = 0 to max do
      print_int i;
      print_string " ";
      match ra.(i) with
	| Some _ -> print_endline "Correctly allocated!"
	| _ -> print_endline "Already deallocated??";
    done;
  end

let _ = print_endline "listo"

let _ = val1 := Base 1

let _ =
  Gc.full_major ();
  Gc.full_major ();
  begin
    for i = 0 to max do
      print_int i;
      print_string " ";
      match ra.(i) with
	| Some _ -> print_endline "Still not deallocated?"
	| _ -> print_endline "Correctly deallocated!"
    done;
  end


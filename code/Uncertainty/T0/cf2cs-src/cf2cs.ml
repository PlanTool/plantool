(*
  For using the interpreter:
  ocaml -I /home/hlp/local/lib/ocaml/site-lib/res res.cma unix.cma str.cma
*)

module MyStrat = struct
  type t = float * float * int

  let default = 1.5, 0.1, 3 (* was 16 in library *)

  let grow (waste, _, min_size) resizer new_len =
    resizer (max (truncate (float new_len *. waste)) min_size)

  let shrink (waste, shrink_trig, min_size) resizer rlen new_len =
    if rlen > min_size && truncate (float rlen *. shrink_trig) > new_len then
      resizer (max (truncate (float new_len *. waste)) min_size)
end

open Format
module MySmallArray = Res.MakeArray(MyStrat)
module MyArray = Res.Array
module MyBits = Res.Bits

let init_hash_size = 0
(* let list_map = List.map *)
let list_map = List.rev_map

(*****************************************************)
(*****************    Interface	  ********************)
(*****************************************************)

let mystdout = ref(stdout)
let print_char c = output_char !mystdout c
let print_string s = output_string !mystdout s
let print_int i = output_string !mystdout (string_of_int i)
let print_float f = output_string !mystdout (string_of_float f)
let print_endline s =
  output_string !mystdout s; output_char !mystdout '\n'; flush !mystdout
let print_newline () = output_char !mystdout '\n'; flush !mystdout

let set_stdout c = mystdout := c
let reset_stdout () = mystdout := stdout

(*****************************************************)
(*****************    Auxiliary	  ********************)
(*****************************************************)

let list_iter_pair f lst =
  let rec aux_l l = function
      [] -> ()
    | l2::lst ->
	f l l2;
	aux_l l lst in
  let rec aux = function
      [] -> ()
    | l::lst ->
  	aux_l l lst;
	aux lst
  in aux lst

(* including L,L *)
let list_iter_pair_same f lst =
  let rec aux_l l = function
      [] -> ()
    | l2::lst ->
	f l l2;
	aux_l l lst in
  let rec aux = function
      [] -> ()
    | l::lst ->
	aux_l l (l::lst);
	aux lst
  in aux lst

let print_list_int lst = 
  print_string "{ ";
  List.iter (fun x -> 
	       print_int x;
	       print_string " ") lst;
  print_endline "}"
    

let false_fun _ = false

let string_starts_with s pre =
  let rec aux i = 
    if i = -1 then
      true
    else (s.[i] = pre.[i])
      && (aux (i-1)) in
  let len_pre = (String.length pre)
  in if len_pre > (String.length s) then
      false
    else
      aux (len_pre-1)

let combs_hits_all llist = 
  let rec aux acc = function
      [] -> acc
    | a::lst ->
	if a == [] then
	  aux acc lst
	else
	  aux
	    (List.concat
	       (list_map
		  (fun ai -> 
		     (list_map (fun li -> ai::li) acc))
		  a))
	    lst
  in match llist with 
      [] -> []
    | a::lst -> 
	aux (list_map (fun l->[l]) a)
	  lst

let combs_hits_one llist =
  let rec aux = function
      [] -> []
    | []::lst ->
	aux lst
    | (init::_)::lst ->
	init::(aux lst) 
  in [aux llist]

module Timed = 
struct
  let report_time_str msg t = 
    begin
      print_string "************* Time elapsed in ";
      print_string msg;
      print_string ": ";
      print_string t;
      print_endline "\n";
    end

  let report_time msg {Unix.tms_utime = u }  =
    report_time_str 
      msg 
      (match Unix.times() 
       with {Unix.tms_utime = u2 } -> string_of_float (u2-.u))

  let call msg f =
    let t = Unix.times() in
    let res = 
      try
	f () 
      with Not_found as a -> 
	print_endline ("Not_found exception raised while calling "^msg);
	raise a
    in begin
	report_time msg t;
	res
      end
end

module OrderedString = 
struct
  type t = string
  let compare = compare 
end
module SetString = Set.Make(OrderedString)
module MapString = Map.Make(OrderedString)

module OrderedInt =
struct
  type t = int
  let compare = compare
end
module MapInt = Map.Make(OrderedInt)

module Verbose = 
struct
  let level0 = 10
  let level1 = 20
  let level2 = 30
  let level = ref(level1)
end

(*****************************************************)
(*****************	Types	  ********************)
(*****************************************************)

(* *)
module type AbstractAtom =
sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
  val to_text: t -> string
  val nil: t
end

module type FACTORY =
sig
  type element
  type t
  val reset : unit -> unit
  val make : element -> t
  val tbl_iter :
    (element -> t -> unit) -> unit
  val tbl_fold :
    (element -> t -> 'a -> 'a) -> 'a -> 'a
  val tbl_mem : element -> bool
  val num_elements : unit -> int
  val get : t -> element
  val num : t -> int 
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_text : t -> string
  val nil : t
end

let use_now_short_names = ref(false)

module Factory (AA:AbstractAtom) : (FACTORY with type element = AA.t) =
struct
  type element = AA.t 
  type t = int * element
  let init_i = 0
  let i = ref init_i (* safe for reentrant is use reset() *)

  module InternalHash = Hashtbl.Make(AA)

  let new_tbl() = InternalHash.create init_hash_size
  (* safe for reentrant is use reset() *)
  let tbl = ref( new_tbl() )

  let reset () =
    begin
      i := init_i;
      tbl := new_tbl();
    end

  let make x = 
    try
      InternalHash.find !tbl x
    with Not_found ->
      begin
	incr i;
	let res = !i, x
   	in 
	  begin
	    InternalHash.add !tbl x res;
 	    res
	  end
      end

  (* In general these function go over
     all used atoms.

     For Factory of KL/t, a function asking whether 
     a KL/t does exist, should use is_tagged,
     and avoid make_atom. 
     It won't case a soundness problem though.
  *)
  let tbl_iter f = 
    InternalHash.iter f !tbl
  let tbl_fold f i =
    InternalHash.fold f !tbl i
  let tbl_mem l =
    InternalHash.mem !tbl l
  let num_elements () = !i
      
  let get ((_, a):t) = a
  let num ((n, _):t) = n

  let compare ((a,_):t) ((b,_):t) = compare a b
  let equal ((a,_):t) ((b,_):t) = a == b
  let hash ((a,_ ):t) = a
  let base_name_l_1 = "bcghjklmqrtuvxyz"
  let base_name_l_2 = "abcdefghijklmnopqsrtuvwxyz1234567890"

  let append_one res src value =
    let i = value mod String.length src
    in (res ^ (String.sub src i 1),
	value / String.length src)

  let c value =
    let rec c1 (acc,value) =
      if value == 0 then
	acc
      else
	c1 (append_one acc base_name_l_2 value)
    in c1 (append_one "" base_name_l_1 value)

  let to_text ((n,a):t) = 
    if !use_now_short_names then (* OJO: global var! BAD *)
      c n
    else
      AA.to_text a

(*   let to_text ((_,a):t) = AA.to_text a *)
(*   let to_text (n,a) = (AA.to_text a)^"-"^(string_of_int n) *)

  let (nil:t) = (0,AA.nil)

end 

module Mylist =
struct
  module T = MySmallArray
  type 'a p = 'a * int
  type 'a t = 'a T.t

  let empty () = T.empty()
  let clear (l: 'a t) = T.clear l
  let length (l: 'a t) = T.length l
  let is_empty (l: 'a t) = length l == 0
  let get (l: 'a t) i = T.get l i
  let set (l: 'a t) i = T.set l i
  let iter f (l: 'a t) =
    T.iter f l
  let iteri f (l: 'a t) =
    T.iteri f l
  let add x (l: 'a t) =
    T.add_one l x

  let swap (l: 'a t) i j =
    T.swap l i j
  let partition f (l: 'a t) = 
    T.partition f l
  let concat (ll: 'a t list) =
    T.concat ll
  let find f l =
    try
      let value = ref(get l 0) (* safe: reentrant *)
      in
	(try
	   T.iter
	     (fun it -> 
		if f it then 
		  begin
		    value := it;
		    raise (Failure "");
		  end)
	     l;
	   raise Not_found
	 with
	     Failure "" -> !value)
    with _ -> raise Not_found

  let fold f init (l: 'a t) =
    T.fold_left f init l
  let append_to (l1: 'a t) (l2: 'a t) =
    iter 
      (fun it2 -> add it2 l1)
      l2
  let map f (l: 'a t) =
    let (res: 'b t) = T.emptyn (length l)
    in iter
	 (fun it -> T.add_one res (f it))
	 l;
      res
  let map_flat f (l: 'a t) =
    let (res: 'b t) = T.emptyn (length l)
    in iter
	 (fun it -> 
	    List.iter 
	      (fun vi -> T.add_one res vi)
	      (f it))
	 l;
      res
  let map_same f (l: 'a t) =
    T.iteri
      (fun i it -> T.set l i (f it))
      l
  let map_from_list f l = 
    let (res: 'b t) = empty()
    in List.iter
	 (fun it -> T.add_one res (f it))
	 l;
      res
  let copy l = 
    map (fun x -> x) l

  (* use carefully. Change order of elements. You index can loose sense. *)
  (* safe strategy for bunch of deletions: from higher index to lower *)
  let remove_i i (l: 'a t) =
    begin
      if i != T.lix l then
	T.set l i (T.get l (T.lix l));
      T.remove_one l;
    end
      
  let rev (l: 'a t) =
    let (res: 'b t) = T.emptyn (length l) 
    in for i = (length l)-1 downto 0 do
	T.add_one res (T.get l i)
      done;
      res

  (* keeps elt st (f elt) *)
  let filter f (l: 'a t) =
    (* Don't use T.filter_in_place because we 
       allow to change the order: more efficient *)
    let index_to_del = ref([]) (* safe: reentrant *)
    in begin
	T.iteri
	  (fun i it -> if not(f it) then
	     index_to_del := i::!index_to_del)
	  l;
	List.iter
	  (fun i -> remove_i i l)
	  !index_to_del;
	  (* It is not necessary to do:
	     (List.rev (List.sort compare !index_to_del))
	     as they come in that order *)
      end
  let filter_copy f (l: 'a t) =
    let res = copy l 
    in T.filter f res
  let filter_and_map f (l: 'a t) =
    let index_to_del = ref([]) (* safe: reentrant *)
    in begin
	T.iteri
	  (fun i it -> 
	     let (keep, value) = f it 
	     in if keep then
		 set l i value
	       else
		 index_to_del := i::!index_to_del
	  )
	  l;
	if false then
	  begin
	    print_string "Index_to_del: ";
	    List.iter 
	      (fun i -> print_int i; print_string " ")
	      !index_to_del;
	    print_newline();
	  end;
	List.iter
	  (fun i -> remove_i i l)
	  !index_to_del;
	  (* It is not necessary to do:
	     (List.rev (List.sort compare !index_to_del))
	     as they come in that order *)
      end
  let exists f (l: 'a t) =
    T.exists f l
  let for_all f (l: 'a t) =
    T.for_all f l
      
  let to_list l =
    fold (fun acc it -> it::acc) [] l 

  let iter_pair_i f (lst: 'a t) =
    let rec aux_l i j = 
      if j < 0 then ()
      else
	begin
	  f i j;
	  aux_l i (j-1)
	end in
    let rec aux j = 
      if j < 0 then ()
      else
	begin
	  aux_l j (j-1);
	  aux (j-1)
	end
    in aux ((length lst)-1)

  let iter_pair f (lst: 'a t) =
    iter_pair_i
      (fun i j -> f (get lst i) (get lst j))
      lst

  let iter_pair_i2 f (lst: 'a t) (lst2: 'a t) =
    let rec aux_l i j = 
      if j < 0 then ()
      else
	begin
	  f i j;
	  aux_l i (j-1)
	end in
    let rec aux i j = 
      if i < 0 then ()
      else
	begin
	  aux_l i j;
	  aux (i-1) j;
	end
    in aux ((length lst)-1) ((length lst2)-1)

  let iter_pair2 f (lst: 'a t) (lst2: 'a t) =
    iter_pair_i2
      (fun i j -> f (get lst i) (get lst2 j))
      lst
      lst2
end

module type AbstractObj =
sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
  val num: t -> int (* Should be >= 0, because can use a repository indexed from 0 *)
  val nil: t
end

(* Small footprint, maybe slow
   Functional.
*)
module SmallSet(AO:AbstractObj) =
struct
  module T = Set.Make(AO)
  type t = T.t
  let empty = T.empty
  let fold = T.fold
  let iter = T.iter
  let add = T.add
  let mem = T.mem
  let remove = T.remove
  let for_all = T.for_all
  let exists = T.exists
  let diff = T.diff
  let cardinal = T.cardinal
  let choose = T.choose
  let filter f s =
    let aux e ns =
      if f e then
	add e ns
      else
	ns
    in T.fold aux s empty
  let remove_list_of l s = 
    let f acc elt = 
      T.remove elt acc
    in List.fold_left f s l
  let add_list_to l s = 
    let f acc elt = 
      T.add elt acc
    in List.fold_left f s l
  let add_mylist_to l s = 
    let f acc elt = 
      T.add elt acc
    in Mylist.fold f s l
  let of_list ll = 
    let acc_list acc l = 
      T.add l acc 
    in List.fold_left acc_list T.empty ll
  let of_mylist ll = 
    let acc_list acc l = 
      T.add l acc 
    in Mylist.fold acc_list T.empty ll
  let list_of sl = 
    T.elements sl
  let mylist_of sl =
    let res = Mylist.empty ()
    in iter (fun elt -> Mylist.add elt res) sl;
      res
end

module IntAO =
struct
  type t = int
  let compare = compare
  let equal x y = (x == y)
  let hash x = x 
  let num x = if x > 0 then 2*x else (-2*x)+1
  let nil = 0
end

module SmallSetInt = SmallSet(IntAO)

module SmallMap(AO:AbstractObj) =
struct
  module T = Map.Make(AO)
  type 'a t = 'a T.t
  let empty = T.empty
  let fold = T.fold
  let iter = T.iter
  let add = T.add
  let mem = T.mem
  let find = T.find
end


module type SPARSESET = 
sig
  type t
  type element
  val cardinal : t -> int
  val length : t -> int (* quickier but inexact *)
  val copy : t -> t
  val empty : unit -> t
  val iter : (element -> unit) -> t -> unit
  val add : element -> t -> unit
  val remove : element -> t -> unit
  val mem : t -> element -> bool
  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a
  val clear : t -> unit
  val fill: t -> int -> unit
  val list_of : t -> element list
  val mylist_of : t -> element Mylist.t
  val of_mylist : element Mylist.t -> t
  val add_list_to : element list -> t -> unit
  val add_mylist_to : element Mylist.t -> t -> unit
  val add_to : t -> t -> unit
  val add_f_to : (element -> element) -> t -> t -> unit
  val map : (element -> element) -> t -> t
  val diff : t -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val subset : t -> t -> bool
  val for_all : (element -> bool) -> t -> bool
end

module type GenSet =
sig
  type t
  type element
  val empty : unit -> t
  val add : element -> t -> unit
  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (element -> unit) -> t -> unit
  val mem : t -> element -> bool
end

module SetOperations(GSBig:GenSet)(GSSmall:GenSet with type element = GSBig.element) =
struct
  let list_of sl =
    let acc_set elt acc =
      elt::acc
    in GSBig.fold acc_set sl []
  let mylist_of sl =
    let res = Mylist.empty ()
    in GSBig.iter (fun elt -> Mylist.add elt res) sl;
      res
  let of_mylist ll = 
    let res = GSBig.empty() in
      Mylist.iter (fun elt -> GSBig.add elt res) ll;
      res

  let add_list_to l s =
    List.iter (fun elt -> GSBig.add elt s) l
  let add_mylist_to l s =
    Mylist.iter (fun elt -> GSBig.add elt s) l
  let add_to src dst =
    GSSmall.iter (fun e -> GSBig.add e dst) src
  let add_f_to f src dst =
    GSSmall.iter (fun e -> GSBig.add (f e) dst) src

  let map f s =
    let res = GSBig.empty() in
      GSBig.iter (fun elt -> GSBig.add (f elt) res) s;
      res

  let diff s1 s2 =
    let res = GSSmall.empty() in
      GSSmall.iter
	(fun k ->
	   if not(GSBig.mem s2 k) then
	     GSSmall.add k res)
	s1;
      res

  let union s1 s2 =
    let res = GSBig.empty() in
      add_to s1 res;
      add_to s2 res;
      res

  let inter s1 s2 =
    let res = GSSmall.empty ()
    in GSSmall.iter
	 (fun k ->
	    if GSBig.mem s2 k then
	      GSSmall.add k res)
	 s1;
      res
	
  let subset s1 s2 =
    try
      GSSmall.iter
	(fun k ->
	   if not(GSBig.mem s2 k) then
	     raise (Failure ""))
	s1;
      true
    with Failure _ ->
      false

  let for_all f s =
    try
      GSSmall.iter
	(fun k ->
	   if not(f k) then
	     raise (Failure ""))
	s;
      true
    with Failure _ ->
      false
end

(* 
   Not really small footprint, but fast...
   Imperative.
   Semantics interchangable with Array versions.
*)
module SetHash(AO:AbstractObj) : (SPARSESET with type element = AO.t) =
struct
  module T = Hashtbl.Make(AO) (* with type 'a = unit *)
  type t = unit T.t
  type element = AO.t
  let length (t:t) = T.length t
  let cardinal = length
  let copy (t:t) = T.copy t
  let empty () = (T.create init_hash_size:t)
  let iter f (t:t) = T.iter (fun v _ -> f v) t
  let fill (t:t) n = ()
  let add x (t:t) = T.replace t x ()
  let remove x (t:t) = T.remove t x
  let mem (t:t) e = T.mem t e 
  let fold f (t:t) init = T.fold (fun k () acc -> f k acc) t init
  let clear (t:t) = T.clear t

  module GenSparse =
  struct
    type element = AO.t
    type t = unit T.t
    let fold = fold
    let add = add
    let iter = iter
    let empty = empty
    let mem = mem
  end
  include SetOperations(GenSparse)(GenSparse)
end

module SetArray(AO:AbstractObj) : (SPARSESET with type element = AO.t) =
struct
  module T = MyArray
  type element = AO.t
  type t = element T.t
  let length (t:t) = T.length t
  let copy (t:t) = T.copy t
  let empty () = (T.empty():t)
  let cardinal (t:t) =
    let s = ref(0) (* safe: reentrant *)
    in T.iter
	 (function v ->
	    if v != AO.nil then
	      incr s)
	 t;
      !s
  let iter f (t:t) =
    T.iter
      (function v ->
	 if v != AO.nil then
	   f v)
      t
  let fill (t:t) n = 
    (* fill_space is a function added to Res library to allow directed growing *)
    T.fill_space t n AO.nil
  let add x (t:t) =  
    assert(AO.num x >= 0);
    fill t (AO.num x);
    T.set t (AO.num x) x
  let remove x (t:t) = 
    assert(AO.num x >= 0);
    fill t (AO.num x);
    T.set t (AO.num x) AO.nil
  let mem (t:t) e = 
    assert(AO.num e >= 0);
    fill t (AO.num e);
    if (T.get t (AO.num e)) != AO.nil then
      begin
	assert (T.get t (AO.num e) == e);
	true
      end
    else
      false
	
  let fold f (t:t) init = 
    T.fold_right
      (fun k acc -> 
	 if k == AO.nil then
	   acc
	 else
	   f k acc)
      t init
  let clear (t:t) = T.clear t

  module GenArray =
  struct
    type element = AO.t
    type t = element T.t
    let fold = fold
    let add = add
    let iter = iter
    let empty = empty
    let mem = mem
  end
  include SetOperations(GenArray)(GenArray)
end

module SparseSet(AO:AbstractObj) = SetHash(AO)
module FastSet(AO:AbstractObj) = SetArray(AO)

module type SPARSEMAP = 
sig
  type 'a t
  type element
  val empty : unit -> 'a t
  val iter : (element -> 'a -> unit) -> 'a t -> unit
  val add : element -> 'a -> 'a t -> unit
  val remove : element -> 'a t -> unit
  val mem : 'a t -> element -> bool
  val find : 'a t -> element -> 'a
  val clear : 'a t -> unit
  val fill: 'a t -> int -> unit
end

module MapHash(AO:AbstractObj) : (SPARSEMAP with type element = AO.t) =
struct
  module T = Hashtbl.Make(AO)
  type 'a t = 'a T.t
  type element = AO.t
  let empty () = T.create init_hash_size
  let iter = T.iter
  let fill t n = ()
  let add x v t = T.replace t x v
  let remove x t = T.remove t x
  let mem = T.mem
  let find = T.find
  let clear = T.clear
end

module MapArray(AO:AbstractObj) : (SPARSEMAP with type element = AO.t) =
struct
  module T = MyArray
  module B = MyBits
  type element = AO.t
  type 'a t = { ar: (element * 'a) T.t; used: B.t }
  let empty () = { ar = T.empty(); used = B.empty() }
  let iter f { ar = ar; used = used } =
    T.iteri
      (fun i (e,v) ->
	 if B.get used i then
	   f e v)
      ar 

  (*
    Use "Obj.magic 0" as nil value for content.
    Obj.magic fools the type system.
    It is safe because those values are never accessed
    unless the boolean array indicates it have a right value.
  *)
  let our_nil = (AO.nil, Obj.magic 0)

  let fill_arrays ar used n = 
    T.fill_space ar n our_nil;
    B.fill_space used n false
  let fill { ar = ar; used = used } n =
    fill_arrays ar used n
  let add x v { ar = ar; used = used } =  
    let n = AO.num x in
      begin
	assert(n >= 0);
	fill_arrays ar used n;
	T.set ar n (x,v);
	B.set used n true
      end
  let remove x { ar = ar; used = used } =
    let n = AO.num x in
      assert(n >= 0);
      fill_arrays ar used n;
      T.set ar n our_nil;
      B.set used n false
  let mem { ar = ar; used = used } e =
    let n = AO.num e in
      assert(n >= 0);
      fill_arrays ar used n;
      B.get used n
  let find { ar = ar; used = used } e =
    let n = AO.num e in
      fill_arrays ar used n;
      if B.get used n then
	match T.get ar n with 
	    _,v -> v
      else
	raise Not_found
  let clear  { ar = ar; used = used } =
    T.clear ar;
    B.clear used
end

module SparseMap(AO:AbstractObj) = MapHash(AO)
module FastMap(AO:AbstractObj) = MapArray(AO)

module OrderedList(AO:AbstractObj) = 
struct
  type t = AO.t list
  let rec compare l1 l2 = match (l1,l2) with
      [],[] -> 0
    | [],_ -> -1
    | _,[] -> 1
    | (a1::l1),(a2::l2) -> 
	let r = AO.compare a1 a2 
	in if r != 0 then r
	  else compare l1 l2
	    
  let equal l1 l2 = (compare l1 l2) == 0
    
  let hash_val = 5381
  let hash v = 
    let rec aux acc = function 
	[] -> acc
      | c::l -> 
	  aux (((acc lsl 5) + acc) + (AO.hash c)) l
    in aux hash_val v
end
  
(*
  Assumes that list are ordered.
  Very slow. Try to use instead a module in KAtom
*)
module SparseSetList(AO:AbstractObj) =
struct
  module OrderedListAO = OrderedList(AO)

  module T = Hashtbl.Make(OrderedListAO)
  type t = unit T.t
  let length (t:t) = T.length t
  let empty () = (T.create init_hash_size:t)
  let iter f (t:t) = T.iter (fun v _ -> f v) t
  let add x (t:t) = T.replace t x ()
  let remove x (t:t) = T.remove t x
  let mem (t:t) e = T.mem t e 
  let fold f (t:t) init = T.fold (fun k () acc -> f k acc) t init
end

(* Probably very slow *)
module OrderedSet(AO:AbstractObj) =
struct
  module SmallSetAO = SmallSet(AO)
  type t = SmallSetAO.t
  let compare = compare
end

(* Map of Set of AO *)
module MapSetAO(AO:AbstractObj) = 
  Map.Make(OrderedSet(AO))
    
(* Set of Set of AO *)
module SetSetAO(AO:AbstractObj) =
  Set.Make(OrderedSet(AO))

module Literal (AA:AbstractAtom) =
struct
  module A = Factory(AA) 

  module L =
  struct 
    type t = P1 of A.t | N1 of A.t
    let hash = function
	P1 o -> A.num(o) lsl 1
      | N1 o -> (A.num(o) lsl 1) lor 1
    let equal x y = (hash x) == (hash y) 
    let compare x y = compare (hash x) (hash y) 
    let to_text = function
	P1 a -> A.to_text a
      | N1 a -> "-"^A.to_text a
    let nil = P1 A.nil
    let num = hash
  end
    
  module F = Factory(L)

  type t = F.t
  let hash = F.hash
  let compare = F.compare 
  let equal = F.equal
  let to_text = F.to_text
  let nil = F.nil

  let print_lit l = 
    match F.get l with
	L.P1 a -> print_string " ("; print_string(A.to_text a); print_string ")"
      | L.N1 a -> print_string " (not("; print_string(A.to_text a); 
	  print_string "))"

  let print_atom a = 
    begin
      print_string "(";
      print_string (A.to_text a); 
      print_string ")";
    end

  let make_atom =
    function a -> A.make a

  module SparseMapL = SparseMap(F)
  module SparseMapA = SparseMap(A)
  module FastMapL = FastMap(F)
  module FastMapA = FastMap(A)

  (*****************************************************)
  (************     Cache               ****************)
  (*****************************************************)
  let use_cache = false (* OJO: before make it true, verify that 
			   these cache are properly reset for reentrant code *)

  module type Func =
  sig
    type t
    val f: F.t -> t
  end

  module Cache(F:Func) =
  struct
    let cache = FastMapL.empty ()
    let reset () = FastMapL.clear cache
    let do_f lit = 
      try
	FastMapL.find cache lit
      with Not_found ->
	let res = F.f lit
	in begin
	    FastMapL.add lit res cache;
	    res;
	  end
  end
  module type FuncA =
  sig
    type t
    val f: A.t -> t
  end
  module CacheA(F:FuncA) =
  struct
    let cache = FastMapA.empty ()
    let reset () = FastMapA.clear cache
    let do_f a = 
      try
	FastMapA.find cache a
      with Not_found ->
	let res = F.f a
	in begin
	    FastMapA.add a res cache;
	    res;
	  end
  end
      
  let calc_pos_atom a = F.make(L.P1 a)
  module Pos_atom = 
    CacheA(
      struct
	type t = F.t
	let f = calc_pos_atom
      end)
  let pos_atom a =
    if use_cache then
      Pos_atom.do_f a
    else
      calc_pos_atom a

  let calc_neg_atom a = F.make(L.N1 a)
  module Neg_atom = 
    CacheA(
      struct
	type t = F.t
	let f = calc_neg_atom
      end)
  let neg_atom a =
    if use_cache then
      Neg_atom.do_f a
    else
      calc_neg_atom a

  let calc_neg_lit l = 
    F.make
      (match F.get l with
	   L.P1 a -> L.N1 a
	 | L.N1 a -> L.P1 a)
  module Neg_lit = 
    Cache(
      struct
	type t = F.t
	let f = calc_neg_lit 
      end)
  let neg_lit lit = 
    if use_cache then
      Neg_lit.do_f lit
    else
      calc_neg_lit lit
      
  let lit_is_neg l = 
    match F.get l with
	L.N1 _ -> true 
      | L.P1 _ -> false 
	  
  let lit_is_pos l = 
    match F.get l with
	L.P1 _ -> true 
      | L.N1 _ -> false
	  
  let trans_atom_in_lit f l = 
    match F.get l with
	L.P1 a -> pos_atom (f a)
      | L.N1 a -> neg_atom (f a)

  (* Try to avoid: deconstruct literal *)
  let atom_of_lit l = 
    match F.get l with
	L.P1 a 
      | L.N1 a -> a

  (* Assume list is sorted! *)
  let rec compare_list l1 l2 = match (l1,l2) with
      [],[] -> 0
    | [],_ -> -1
    | _,[] -> 1
    | (a1::l1),(a2::l2) -> 
	let r = compare a1 a2 
	in if r != 0 then r
	  else compare_list l1 l2

  let compare_list_unsorted l1 l2 =
    let ls1 = List.sort compare l1
    and ls2 = List.sort compare l2 
    in compare_list ls1 ls2

  (* Assume list is sorted! *)
  let compare_mylist l1 l2 =
    let rec get_value = function
	i when i < 0 -> 0
      | i -> 
	  let v = compare (Mylist.get l1 i) (Mylist.get l2 i) 
	  in if v != 0 then
	      v
	    else get_value (i-1)
    in if Mylist.length l1 < Mylist.length l2 then
	-1
      else if Mylist.length l2 < Mylist.length l1 then
	1
      else
	get_value ((Mylist.length l1)-1)

  let included_in it1 l2 =
    Mylist.exists
      (fun it2 -> it1 = it2)
      l2
  let compare_mylist_unsorted l1 l2 =
    if Mylist.length l1 < Mylist.length l2 then
      -1
    else if Mylist.length l2 < Mylist.length l1 then
      1
    else	
      let v1 = Mylist.fold 
	(fun acc it1 -> if included_in it1 l2 then acc+1 else acc)
	0 l1 in
      let v2 = Mylist.fold 
	(fun acc it2 -> if included_in it2 l1 then acc+1 else acc)
	0 l2 
      in v1 - v2

end


module Problem (AA:AbstractAtom) =
struct

  module Lit = Literal(AA)

  let reset () = 
    begin
      Lit.A.reset();
      Lit.F.reset();
    end

  (*****************************************************)
  (*********  Data structures for Atoms    *************)
  (*****************************************************)
    
  module SmallSetA = SmallSet(Lit.A)
  module SmallMapA = SmallMap(Lit.A)
  module SparseSetA = SparseSet(Lit.A)
(*   module SparseMapA = Lit.SparseMap (* not used yet - uncomment in needed *) *)
  module FastSetA = FastSet(Lit.A)
(*   module FastMapA = Lit.FastMap (* not used yet - uncomment in needed *) *)

(*   module MapSetA = MapSetAO(Lit.A) (* not used yet - uncomment in needed *) *)

  (*****************************************************)
  (*********  Data structures for Lits     *************)
  (*****************************************************)

  module SmallSetL = SmallSet(Lit.F)
  module SmallMapL = SmallMap(Lit.F)
  module SparseSetL = SparseSet(Lit.F)
  module SparseMapL = Lit.SparseMapL
  module FastSetL = FastSet(Lit.F)
  module FastMapL = Lit.FastMapL

  module SetL_SmallVsBig = SetOperations(FastSetL)(SparseSetL)
  module SetA_SmallVsBig = SetOperations(FastSetA)(SparseSetA)

  module MapSetL = MapSetAO(Lit.F)
  module SetSetL = SetSetAO(Lit.F)
(*   module SparseSetListL = SparseSetList(Lit.F) *)  (* not used yet - uncomment in needed *)

  (*****************************************************)
  (************     Problem             ****************)
  (*****************************************************)
    
  type condeffect =
      { mutable ccomment: string;
	mutable cond: Lit.t Mylist.t;
	mutable ceff: Lit.t Mylist.t }
  type action = 
      { aname: string;
	mutable prec: Lit.t Mylist.t; 
	mutable eff: Lit.t Mylist.t; 
	mutable conds: condeffect Mylist.t }
  type state_item = Clause of Lit.t list | Single of Lit.t
  type problem =
      { mutable domain: string;
	mutable instance: string;
	mutable init: state_item list;
	mutable goal: state_item list;
	mutable actions: action Mylist.t }

  (*****************************************************)
  (**************   Temporary References   *************)
  (*****************************************************)

  (* all NOT safe for reentrant code. Use once *)
  let condeff_list = ref (Mylist.empty(): condeffect Mylist.t)
  let empty_cond = 
    { ccomment = "";
      cond = Mylist.empty();
      ceff = Mylist.empty() }

  let actions_list = ref (Mylist.empty(): action Mylist.t) 
  let init = ref ([]: state_item list)
  let goal = ref ([]: state_item list)
  let empty_problem () = 
    { domain = ""; 
      instance = ""; 
      init = []; 
      goal = []; 
      actions = Mylist.empty() } 
  let problem = ref (empty_problem (): problem)
  let problem_created = ref(false)

  let loaded_actions = ref(SetString.empty)

  let atoms_problem = ref (MyArray.empty())

  (*****************************************************)
  (**************      Selectors	   *************)
  (*****************************************************)

  let get_ccomment { ccomment = com } = com
  let get_cond { cond = c } = c
  let get_ceff { ceff = ce } = ce

  let get_aname { aname = n } = n
  let get_prec { prec = p } = p
  let get_eff { eff = e } = e
  let get_conds { conds = cs } = cs

  let get_domain () = !problem.domain
  let get_instance () = !problem.instance
  let get_init () = !problem.init
  let get_goal () = !problem.goal
  let get_actions () = !problem.actions

  let set_domain p d = p.domain <- d
  let set_instance p i = p.instance <- i
  let set_init p ni = p.init <- ni
  let set_goal p ng = p.goal <- ng
  let set_actions p a = p.actions <- a


  (*****************************************************)
  (**************   Operations        	  **************)
  (*****************************************************)

  let set_lit_to_set_atom s =  
    let res = FastSetA.empty ()
    in FastSetL.iter  
         (fun e -> FastSetA.add (Lit.atom_of_lit e) res) 
	 s;
      res

  let neg_set_atoms atoms =  
    let res = FastSetL.empty ()
    in FastSetA.iter
	 (fun e -> FastSetL.add (Lit.neg_atom e) res)
	 atoms;
      res

  let neg_set_lit lits =  
    let res = FastSetL.empty ()
    in FastSetL.iter
	 (fun e -> FastSetL.add (Lit.neg_lit e) res)
	 lits;
      res

  let list_lit_from_state state =	      
    List.fold_left 
      (fun acc elt -> 
	 match elt with 
	     Single l -> l::acc
	   | Clause _ -> acc)
      []
      state

  (* OJO: factor out with the next one? *)
  let small_set_lit_from_state state =	      
    List.fold_left 
      (fun acc elt -> 
	 match elt with 
	     Single l -> SmallSetL.add l acc
	   | Clause _ -> acc)
      SmallSetL.empty 
      state

  (* not in clause *)
  let fastset_lit_from_state state =	      
    let res = FastSetL.empty ()
    in List.iter 
	 (fun elt -> 
	    match elt with 
		Single l -> FastSetL.add l res
	      | Clause _ -> raise 
		  (Failure "simulation actions in problem with clauses in state"))
	 state;
      res
	
  let sparseset_lit_from_state state =	      
    let res = SparseSetL.empty ()
    in List.iter 
	 (fun elt -> 
	    match elt with 
		Single l -> SparseSetL.add l res
	      | Clause _ -> raise 
		  (Failure "simulation actions in problem with clauses in state"))
	 state;
      res
	
  let set_from_state state =	      
    let set = FastSetL.empty ()
    in List.iter
	 (fun elt -> 
	    match elt with 
		Single l -> FastSetL.add l set 
              | Clause lst -> ())
	 state;
      set

  let set_from_state_and_clauses state =	      
    let set = FastSetL.empty ()
    in List.iter
	 (fun elt -> 
	    match elt with 
		Single l -> FastSetL.add l set 
              | Clause lst -> 
		  FastSetL.add_list_to lst set)
	 state;
      set

  let is_some_lit_in_set l s =
    try
      Mylist.iter 
	(fun k ->
	   if FastSetL.mem s k then
	     raise (Failure ""))
	l;
      false
    with Failure _ ->
      true

  let is_some_lit_in_sparseset l s =
    try
      List.iter 
	(fun k ->
	   if SparseSetL.mem s k then
	     raise (Failure ""))
	l;
      false
    with Failure _ ->
      true

  let consistent_set_lit s =
    SmallSetL.for_all
      (fun elt1 -> 
	 SmallSetL.for_all (fun elt2 -> elt2 != (Lit.neg_lit elt1)) s)
      s 

  let set_lit_add_to_list s elt value = 
    let old_val = 
      (try
	 SparseMapL.find s elt
       with Not_found -> []) 
    in SparseMapL.add elt (value::old_val) s

  let fast_set_lit_add_to_ints s elt value =
    let old_val =
      (try
	 FastMapL.find s elt
       with Not_found -> SmallSetInt.empty)
    in FastMapL.add elt (SmallSetInt.add value old_val) s
	 
  let fast_set_lit_remove_from_ints s elt value =
    let old_val = FastMapL.find s elt
    in FastMapL.add elt (SmallSetInt.remove value old_val) s
	 
  let get_clause_list acc = function
      Clause l -> l::acc
    | Single _ -> acc

  let get_all_clause_list acc = function
      Clause l -> l::acc
    | Single x -> [x]::acc

  let get_state_list_from_list acc = function
    | [] -> raise (Failure "Error while converting list of clauses into state")
    | [l] -> (Single l)::acc
    | l -> (Clause l)::acc

  let get_clause_set acc = function
      Clause l -> (SmallSetL.of_list l)::acc
    | Single _ -> acc


  (*****************************************************)
  (**************   Recalculate atoms 	  **************)
  (*****************************************************)

  module SaveAtoms =
  struct
    let empty () = FastSetA.empty ()
    let new_atoms = ref(empty()) (* safe as reset by "atoms_of_actions" *)
    let add_atom a = FastSetA.add a !new_atoms
    let proc_lit lit =
      add_atom (Lit.atom_of_lit lit)
  end

  let atoms_of_actions actions =
    let proc_cond condeff =
      begin
	Mylist.iter SaveAtoms.proc_lit condeff.cond;
	Mylist.iter SaveAtoms.proc_lit condeff.ceff;
      end in
    let proc_act act =
      begin
	Mylist.iter SaveAtoms.proc_lit act.prec;
	Mylist.iter SaveAtoms.proc_lit act.eff;
	Mylist.iter proc_cond act.conds;
      end
    in begin
	SaveAtoms.new_atoms := SaveAtoms.empty ();
	Mylist.iter proc_act actions;
	!SaveAtoms.new_atoms;
      end

  let recalculate_atoms_cache_dirty = ref(true)
  let problem_used = ref (empty_problem (): problem)
  let next_recalculate_atoms () = recalculate_atoms_cache_dirty := true
  let recalculate_atoms myproblem =
    let proc_state_item = function
	Single l -> SaveAtoms.proc_lit l
      | Clause lst -> List.iter SaveAtoms.proc_lit lst
    in if !recalculate_atoms_cache_dirty || myproblem != (!problem_used) then
	begin
	  SaveAtoms.new_atoms := atoms_of_actions myproblem.actions;
	  List.iter proc_state_item myproblem.init;
	  List.iter proc_state_item myproblem.goal;
	  let size = FastSetA.length !SaveAtoms.new_atoms
	  in begin
	      atoms_problem := MyArray.create size Lit.A.nil;
	      problem_used := myproblem;
	      recalculate_atoms_cache_dirty := false;
	      ignore(FastSetA.fold
                       (fun a i -> MyArray.set !atoms_problem i a; i+1)
		       !SaveAtoms.new_atoms
		       0);
	    end
	end

  let get_atoms_set myproblem =
    recalculate_atoms myproblem;
    !SaveAtoms.new_atoms

  let get_all_lits myproblem = 
    let all_atoms = get_atoms_set myproblem in
    let proc_atom a acc =
      [Lit.pos_atom a; Lit.neg_atom a]@acc
    in FastSetA.fold proc_atom all_atoms []
      
  (*****************************************************)
  (*****************	Printing    ********************)
  (*****************************************************)

  let print_tbl_atom () =
    print_string "{ ";
    Lit.A.tbl_iter (fun _ x -> 
		    print_string (Lit.A.to_text x); 
		    print_string " ");
    print_string "}\n"
      
  let print_fastset_atom s =
    print_string "{ ";
    FastSetA.iter (fun k -> 
		       print_string (Lit.A.to_text k); 
		       print_string " ") s;
    print_string "}\n"
      
  let print_set_atom s =
    print_string "{ ";
    SmallSetA.iter (fun x -> 
		      print_string (Lit.A.to_text x); 
		      print_string " ") s;
    print_string "}\n"
      
  let print_list_atom l =
    print_string "{ ";
    List.iter (fun x -> 
		 print_string (Lit.A.to_text x); 
		 print_string " ") l;
    print_string "}\n"
      
  let print_set_atom_filtered filter s =
    print_string "{ ";
    SmallSetA.iter (fun x -> if (filter x) then 
		      print_string (Lit.A.to_text x); 
		      print_string " ") s;
    print_string "}\n"
      
  let print_fastset_lit s = 
    print_string "{";
    FastSetL.iter (fun k -> Lit.print_lit k) s;
    print_string "}\n"

  let print_sparseset_lit s = 
    print_string "{";
    SparseSetL.iter (fun k -> Lit.print_lit k) s;
    print_string "}\n"

  let print_set_lit s =
    print_string "{";
    SmallSetL.iter Lit.print_lit s;
    print_string "}\n"

  let print_set_lit_filtered filter s =
    print_string "{";
    SmallSetL.iter (fun x -> if (filter x) then Lit.print_lit x) s;
    print_string "}\n"

  let print_list_lit_filterd filter s =
    print_string "{";
    List.iter (fun x -> if (filter x) then Lit.print_lit x) s;
    print_string "}\n"

  let print_list_lit s =
    print_string "{";
    List.iter Lit.print_lit s;
    print_string "}\n"

  let print_mylist_lit s =
    print_string "{";
    Mylist.iter Lit.print_lit s;
    print_string "}\n"

  let print_state_item = function
      Clause [] -> print_string ";; <empty>"; print_newline()
    | Clause [l] | Single l -> Lit.print_lit l; print_newline()
    | Clause (h::l) -> 
	begin
	  print_string "(or";
	  Lit.print_lit h; 
	  List.iter Lit.print_lit l; 
	  print_string ")";
	  print_newline();
	end

  let print_pos_state_item = function
      (Single lit) as item -> 
	if Lit.lit_is_pos lit then
	  print_state_item item
    | (Clause _) as item -> print_state_item item

  let print_state list =
    List.iter print_state_item list
      
  let print_and force_and field = 
    if force_and or Mylist.length field != 1 then
      print_string "(and";
    if not (Mylist.is_empty field) then
      Mylist.iter Lit.print_lit field;
    if force_and or Mylist.length field != 1 then
      print_string ")"

  let print_cond_eff { ccomment = comment; cond = ncond; ceff = nceff } =
    begin
      print_string "(when";
      print_and false ncond;
      print_string " ";
      print_and false nceff;
      print_string ") ;;";
      print_string comment;
      print_newline();
    end

  let print_action have_cost { aname = name; prec = prec; eff = eff; conds = conds } =
    if( Mylist.is_empty eff && Mylist.is_empty conds ) then
      begin
	print_string ";;Omitted action ";
	print_string name;
	print_endline " because didn't have effect or cond_effects";
      end
    else
      begin
	print_string "(:action ";
	print_string name;
	print_string "\n:parameters ()";
	print_string "\n:precondition ";
	print_and true prec;
	print_string "\n\n:effect (and ";
	if have_cost name then
	  print_string "(increase (spent) (act))\n";
	Mylist.iter Lit.print_lit eff;
	print_newline();
	Mylist.iter print_cond_eff conds;
	print_string "))";
	print_newline();
      end

  let print_problem { domain = domain; instance = instance; 
		      init = init; goal = goal } =
    begin
      print_string "Domain ";
      print_string domain;
      print_string ". Problem ";
      print_string instance;
      print_string ": \nInit: (and \n";
      print_state init;
      print_string ") Goal: (and \n";
      print_state goal;
      print_string ")";
      print_newline();
    end

  let print_predicates () =
    begin
      FastSetA.iter 
	(fun a -> 
	   Lit.print_atom a;
	   print_newline())
	(get_atoms_set !problem);
      if !problem.init == [] then
      	print_endline " (init-never-empty) ";
    end
      
  let print_pddl_cost base_name use_cost have_cost =
    let dfile = open_out (base_name^"-d.pddl")
    and pfile = open_out (base_name^"-p.pddl") 
    in begin
	set_stdout dfile;
	print_string "(define (domain ";
	print_string !problem.domain;
	print_string ")\n";
	print_string "(:requirements :negative-preconditions :conditional-effects";
	if use_cost then
	  print_string " :fluents";
	print_string ")\n";
	print_string "\t(:predicates\n";
	print_predicates();
	print_string ")\n";
	if use_cost then
	  print_string "(:functions (spent) (act))\n";
	Mylist.iter (print_action 
		       (fun aname -> use_cost && have_cost aname))
	  !problem.actions;
	print_string ")\n";
	close_out dfile;

	set_stdout pfile;
	print_string "(define (problem ";
	print_string !problem.instance;
	print_string ")\n\t(:domain ";
	print_string !problem.domain;
	print_string ")\n(:init \n";
	if !problem.init == [] then
	  print_endline " (init-never-empty) ";
	List.iter print_pos_state_item !problem.init;
	if use_cost then
	  print_string "(= (spent) 0) (= (act) 1)\n";
	print_string ")\n(:goal (and \n";
	print_state !problem.goal;
	print_string "))\n";
	if use_cost then
	  print_string "(:metric minimize (spent))\n";
	print_string  ")";
	print_newline();
	close_out pfile;

	reset_stdout();
      end

  let print_pddl base_name =
    print_pddl_cost base_name false false_fun


  let print_short_long_table () = 
    begin
      print_endline "Short to Long atoms translation table";
      print_endline "=====================================";
      FastSetA.iter 
	(fun a -> 
	   use_now_short_names := true;
	   Lit.print_atom a; 
	   use_now_short_names := false;
	   print_string " -> ";
	   Lit.print_atom a; 
	   print_newline())
	(get_atoms_set !problem);
      print_endline "=====================================";
    end

  (*****************************************************)
  (**************    Post-processing	  **************)
  (*****************************************************)

  (* require atoms to be set.
     Atom not mentioned in clause neither literal.
  *)

  let atom_to_trivial_clauses_set atoms =
    let proc_trivial u  acc =
      (Clause [Lit.pos_atom u; Lit.neg_atom u])::acc
    in FastSetA.fold proc_trivial atoms []
	 
  (* for each action
     -- make map of int list to set of (comment, int list)
     then, remake actions

     Fast enough, even when using MapSetL
  *)
  let compact_conds { domain = ldomain; instance = linstance;
		      init = linit; goal = lgoal; actions = lactions } =
    let get_comments comset =
      SetString.fold (fun s acc -> s^". "^acc) comset "" in
    let new_action act = 
      let map = ref(MapSetL.empty) (* safe: reentrant *) in
      let fill_a_cond { ccomment = com; cond = cond; ceff = ceff } =
	let (oldsetcom,oldsetceff) =
	  try
	    MapSetL.find (SmallSetL.of_mylist cond) !map
	  with Not_found ->
	    (SetString.empty, SmallSetL.empty)
	in map := MapSetL.add
	     (SmallSetL.of_mylist cond)
	     (SetString.add com oldsetcom,
	      SmallSetL.add_mylist_to ceff oldsetceff)
	     !map in
      let fill_map () =
	Mylist.iter fill_a_cond act.conds in
      let dump_map () =
	let res = Mylist.empty () in
	let proc_key key (comset,litset) =
	  Mylist.add 
	    { ccomment = get_comments comset;
	      cond = SmallSetL.mylist_of key;
	      ceff = SmallSetL.mylist_of litset
	    }
	    res
       	in MapSetL.iter proc_key !map;
	  res
      in begin
	  fill_map ();
	  act.conds <- dump_map ();
	end in
    let _ = Mylist.iter new_action lactions
    in { domain = ldomain;
	 instance = linstance;
	 init = linit;
	 goal = lgoal;
	 actions = lactions }
	 
  (*****************************************************)
  (**************  Reachability analysis  **************)
  (*****************************************************)

  let debug_reachability = false
  let debug_reachability2 = false
  let debug_reachability3 = false

  let print_current s set =
    begin
      print_string s;
      print_string " ";
      print_fastset_lit set;
    end

  let init_modified_and_used = function
      { init = linit; goal = lgoal; actions = lactions } as problem ->
	(*
	   Init also require explicitly the negative literals,
	   because we also hace negative conditions.
	   Otherwise the would appear as not applied.
	*)
	let atoms = get_atoms_set problem in
	let init_from_linit = set_from_state_and_clauses linit in
	let sure_init_from_linit = set_from_state linit in
	let atoms_from_linit = set_lit_to_set_atom init_from_linit in
	let not_mentioned_from_linit = FastSetA.diff atoms atoms_from_linit in
	let neg_atoms_not_mentioned = neg_set_atoms not_mentioned_from_linit in
	let init_reached = FastSetL.union init_from_linit neg_atoms_not_mentioned in
	let sure_init_reached = FastSetL.union sure_init_from_linit neg_atoms_not_mentioned in
	  if debug_reachability2 then begin
	    print_string "FINAL FROM INIT: ";
	    print_fastset_lit init_reached;
	    print_newline();
	  end;

	  let modified = FastSetL.empty ()
	  and used = FastSetL.empty ()
	  and cmodified = FastSetL.empty ()
	  and cused = FastSetL.empty () in
	  let num_pass_fixpoint = ref(1) (* safe: reentrant *) in
	  let rec fixpoint () =
	    let is_reached l =
	      Mylist.for_all
		(fun lit ->
		   FastSetL.mem modified lit ||
		     FastSetL.mem cmodified lit ||
		     FastSetL.mem init_reached lit)
		l in
	    let rec each_action act =
	      if debug_reachability3 then
		begin
		  print_string "Fixpoint seeing: ";
		  print_endline act.aname;
		end;
	      let rec each_cond ce =
		if debug_reachability3 then
		  begin
		    print_string "condition: ";
		    print_mylist_lit ce.cond;
		  end;
                if not (Mylist.is_empty ce.ceff) &&
		  is_reached ce.cond then
		    begin
		      FastSetL.add_mylist_to ce.ceff cmodified;
		      (* both prec and cond were used *)
		      FastSetL.add_mylist_to act.prec cused;
		      FastSetL.add_mylist_to ce.cond cused;
		      if debug_reachability3 then
			begin
			  print_current "cused" cused;
			  print_current "cmodified" cmodified;
			end;
		    end
	      in if is_reached act.prec then
		  begin
		    if debug_reachability3 then
		      begin
			print_string "Fixpoint entering: ";
			print_endline act.aname;
		      end;
		    if not (Mylist.is_empty act.eff) then
		      begin
			FastSetL.add_mylist_to act.eff cmodified;
			FastSetL.add_mylist_to act.prec cused;
		      end;
		    Mylist.iter each_cond act.conds;
		  end
	    in begin
		Mylist.iter each_action lactions;
		if not(FastSetL.subset cmodified modified) ||
		  not(FastSetL.subset cused used) then
		    begin
		      if debug_reachability then
			begin
			  print_string "Calling fixpoint. Size change for cmodified was ";
			  print_int (FastSetL.length cmodified);
			  print_string ". For cused was ";
			  print_int (FastSetL.length cused);
			  print_newline();
			end;
		      FastSetL.add_to cmodified modified;
		      FastSetL.clear cmodified;
		      FastSetL.add_to cused used;
		      FastSetL.clear cused;
		      if debug_reachability3 then
			begin
			  print_current "used" used;
			  print_current "modified" modified;
			end;
		      incr num_pass_fixpoint;
		      fixpoint ()
		    end
	      end
	  in begin
	      fixpoint ();
	      print_string "Fixpoint for reachability finished. Pass ";
	      print_int !num_pass_fixpoint;
	      print_endline " times";
	      let all_reached = FastSetL.union init_reached modified in
	      let goal_hash = fastset_lit_from_state lgoal in
	      let goal_reachable = FastSetL.subset goal_hash all_reached in
		begin
		  if not goal_reachable then
		    begin
		      if false then
			begin
			  print_endline "============== All reached: ";
			  print_fastset_lit all_reached;
			  print_endline "============== Goal: ";
			  print_fastset_lit goal_hash;
			end;
 		      print_endline "goal unreachable. Problem unsolvable";
		    end;
		  FastSetL.add_to goal_hash used;
		  (sure_init_reached,init_reached,modified,used,goal_reachable)
		end
	    end
	       
  let back_used { goal = lgoal; actions = lactions } =
    (*
      Saying that an atom p is used, means that it could have
      support the goal, or a precondition or condition.
      But, also need to consider atoms than block the
      goal, preconds and conditions.
      So, in general, it reports more atoms than the forward
      method, but excludes obvious irrelevant atoms in init.

      It can be done better, probably.
    *)
    let used = FastSetL.empty ()
    and cused = fastset_lit_from_state lgoal in

    let add_list_and_neg_lit_to_hash l s =
      begin
	FastSetL.add_mylist_to l s;
	Mylist.iter
	  (fun elt -> FastSetL.add (Lit.neg_lit elt) s)
	  l
      end in

    let num_pass_fixpoint = ref(1) (* safe: reentrant *) in
    let rec fixpoint () =
      let is_used l  =
	(is_some_lit_in_set l cused) ||
	  (is_some_lit_in_set l used) in
      let rec each_action act =
	let rec each_cond ce =
	  if is_used ce.ceff then
	    begin
	      add_list_and_neg_lit_to_hash act.prec cused; (* both prec and cond were used *)
	      add_list_and_neg_lit_to_hash ce.cond cused;
	    end
	in begin
	    if is_used act.eff then
	      add_list_and_neg_lit_to_hash act.prec cused;
	    Mylist.iter each_cond act.conds
	  end
      in begin
	  Mylist.iter each_action lactions;
	  if FastSetL.subset cused used then
	    used
	  else
	    begin
	      if debug_reachability then
		begin
		  print_string "Calling fixpoint. Size change for backused was ";
		  print_int (FastSetL.length cused);
		  print_newline();
		end;
	      FastSetL.add_to cused used;
	      FastSetL.iter
		(fun k -> FastSetL.add (Lit.neg_lit k) used)
		cused;
	      FastSetL.clear cused;
	      incr num_pass_fixpoint;
	      fixpoint ();
	    end
	end
    in begin
	FastSetL.iter
	  (fun k  ->
	     FastSetL.add k used;
	     FastSetL.add (Lit.neg_lit k) used)
	  cused;
	FastSetL.clear cused;
	let res = fixpoint () in
	print_string "Fixpoint for reachability finished. Pass ";
	print_int !num_pass_fixpoint;
	print_endline " times";
	res
      end


  let calc_reachability problem =
    let (sure_init,init,modified,used,goal_reachable) = 
      Timed.call "specific reachability: init_modified_and_used"
	(fun () -> init_modified_and_used problem) in
    let bused = 
      Timed.call "specific reachability: back_used"
	(fun () -> back_used problem) in
    let neg_modified = neg_set_lit modified in
    let init_always =
      FastSetL.diff
	sure_init
	(FastSetL.union modified neg_modified) in
      
    (* For consistency: if p & -p can't be \in always *)
    let always = FastSetL.diff init_always (neg_set_lit init_always) in
      
    let real_used = FastSetL.inter bused used in
    let real_used_atoms = set_lit_to_set_atom real_used in
      
    let all_used = FastSetL.union init used in
    let all_used_atoms = set_lit_to_set_atom all_used in
      
    let discarted_atoms =
      FastSetA.diff all_used_atoms real_used_atoms
    in begin
	if debug_reachability2 then
	  begin
	    print_endline "\nInit lits: ";
	    print_fastset_lit init;
	    print_endline "\nModified lits: ";
	    print_fastset_lit modified;
	    print_endline "\nUsed lits: ";
	    print_fastset_lit used;
	    print_endline "\nBack used lits: ";
	    print_fastset_lit bused;
	    print_newline();
	    print_endline "\nDiscarted from Used lits: ";
	    print_fastset_lit (FastSetL.diff used real_used);
	    print_endline "\nDiscarted from Back Used lits: ";
	    print_fastset_lit (FastSetL.diff bused real_used);
	    print_newline();
	    print_endline "\nAlways lits: ";
	    print_fastset_lit always;
	    print_endline "\nReal used atoms: ";
	    print_fastset_atom real_used_atoms;
	    print_endline "\nDiscarted atom: ";
	    print_fastset_atom discarted_atoms;
	  end;
	print_string "\nNum of Always lits: ";
	print_int (FastSetL.length always);
	print_string "\nNum of Real used atoms: ";
	print_int (FastSetA.length real_used_atoms);
	print_string "\nNum of Discarted atoms: ";
	print_int (FastSetA.length discarted_atoms);
	print_newline ();
	assert(FastSetA.length (FastSetA.inter real_used_atoms discarted_atoms) = 0);
	(always,real_used_atoms,discarted_atoms,goal_reachable);
      end

  (*
    OJO ???: It seems that a case of simplification is still not used.
     If and support rule for K-L/t is not used,
     it's cancellation is not requiered.
  *)
  let clean_problem_by_reachability problem = 
    let (always_lit,used_atoms,_,goal_reachable) = (*  discarted_atoms *)
      Timed.call "specific reachability"
	(fun () -> calc_reachability problem) in
      (* OJO. If not reachable, we can return right now... *)
    let do_not_discard_lit lit =
      FastSetA.mem used_atoms (Lit.atom_of_lit lit) in
    let do_discard_lit lit =
      not(do_not_discard_lit lit) in
    let neg_always_lit = FastSetL.map Lit.neg_lit always_lit in
    let do_contradict_always lit =
      FastSetL.mem neg_always_lit lit in
    let do_always lit =
      FastSetL.mem always_lit lit in
    let not_always lit =
      not(do_always lit) in
    let remove_always l = (* remove always true *)
      Mylist.filter not_always l in
    let remove_discarted l = (* remove discarted *)
      Mylist.filter do_not_discard_lit l in
      
    (* New cond *)
    let new_cond ce =
      if ((Mylist.exists do_contradict_always ce.cond) ||
	    (Mylist.exists do_discard_lit ce.cond) ) then
	(* discard action because should discard lit
	   or contradict static literal *)
	(false, ce)
      else let _ = remove_discarted ce.ceff in
	if Mylist.is_empty ce.ceff then
	  (false, ce)
	else
	  begin
	    remove_always ce.cond;
	    (true, ce)
	  end in
      
    (* New actions *)
    let new_action act =
      if ((Mylist.exists do_contradict_always act.prec)
	  || (Mylist.exists do_discard_lit act.prec)) then
	
	(* discard action because should discard lit
	   or contradict static literal *)
	(false,act)
      else begin
	Mylist.filter_and_map new_cond act.conds;
	remove_discarted act.eff;
	if Mylist.is_empty act.conds && Mylist.is_empty act.eff then
	  (false, act)
	else
	  begin
	    remove_always act.prec;
	    (true, act)
	  end 
      end in
    let new_actions actions =
      Mylist.filter_and_map new_action actions in
      
    let not_discard_and_not_always l =
      do_not_discard_lit l &&
	not_always l in
      
    (* For init *)
    let keep_item = function
	Clause c ->
	  List.exists not_discard_and_not_always c
      | Single l ->
	  not_discard_and_not_always l in
    let new_init init =
      List.filter keep_item init
	
    (* new problem *)
    in
      begin
	new_actions problem.actions;
	problem.init <- new_init problem.init;
	(* OJO OJO OJO. Delicado y mal: el cache de recalculate atoms  *)
	(* no está ligado al resultado. *)
	next_recalculate_atoms ();
	goal_reachable
      end
	
  (*****************************************************)
  (**************   Modifying Functions	  **************)
  (*****************************************************)
	       
  let debug_load_problem = false

  (*** all NOT safe for reentrant code. Use once ***)

  (* make space for n atoms *)
  let init_problem n =
    begin
      if debug_load_problem then
	begin
	  print_string "init_problem: ";
	  print_int n;
	  print_newline();
	end;
      problem := empty_problem ();
      condeff_list := Mylist.empty ();
      actions_list := Mylist.empty ();
      init := [];
      goal := [];
      atoms_problem := MyArray.create n Lit.A.nil;
      loaded_actions := SetString.empty
    end

  (* Load one atom *)
  let fill_atom n s =
    begin
      MyArray.set !atoms_problem n (Lit.make_atom(s));
      if debug_load_problem then
	begin
	  print_string "fill: ";
	  print_int n;
	  print_string (AA.to_text s);
	  print_newline();
	end;
    end
      
  let trans_n_atom n =
    assert(n < MyArray.length !atoms_problem);
    let atom = MyArray.get !atoms_problem (abs n) in
      if n > 0 then
	Lit.pos_atom atom
      else
	Lit.neg_atom atom

  let array_atom_to_list l =
    list_map trans_n_atom (Array.to_list l)

  let array_atom_to_mylist l =
    Mylist.map_from_list trans_n_atom (Array.to_list l)

  let add_problem_lit where n =
    assert(n < MyArray.length !atoms_problem);
    where := Single(trans_n_atom n)::!where

  let add_init_lit n =
    assert(n < MyArray.length !atoms_problem);
    add_problem_lit init n

  let add_goal_lit n =
    (* assert(n < Array.length !atoms_problem); *)
    if n < MyArray.length !atoms_problem then
      add_problem_lit goal n

  let add_problem_clause where c =
    where := Clause(array_atom_to_list c)::!where

  let add_init_clause c =
    assert(Array.length c > 1);
    if false then
      begin
	print_string "!!**!!** Adding clauses: ";
	Array.iter (fun i -> print_int i; print_string " ") c;
	print_newline();
      end;
    add_problem_clause init c

  let add_goal_clause c =
    assert(Array.length c > 1);
    if false then
      begin
	print_string "!!**!!** Adding clauses GOAL: ";
	Array.iter (fun i -> print_int i; print_string " ") c;
	print_newline();
      end;
    add_problem_clause goal c

  let create_cond_effect comment cond ceff =
    let ncond = array_atom_to_mylist cond
    and nceff = array_atom_to_mylist ceff in
    let c = { ccomment = comment;
	      cond = ncond;
	      ceff = nceff }
    in begin
	if debug_load_problem && false then
	  print_cond_eff c;
	Mylist.add c !condeff_list;
      end
	 
  (* Load all cond effects created, and clean them *)
  let create_action name prec eff =
    let a = { aname = name;
	      prec = array_atom_to_mylist prec;
	      eff = array_atom_to_mylist eff;
	      conds = !condeff_list }
    in begin
	if SetString.mem name !loaded_actions then
	  begin
	    print_endline "ERROR: actions should have different name";
	    raise (Failure "actions should have different name");
	  end
	else
	  loaded_actions := SetString.add name !loaded_actions;
	if debug_load_problem then
	  print_action (fun _ -> false) a;
	Mylist.add a !actions_list;
	condeff_list := Mylist.empty ();
      end
	 
  let create_problem domain instance =
    begin
      problem :=
	{ domain = domain;
	  instance = instance;
	  init = !init;
	  goal = !goal;
	  actions = !actions_list };
      problem_created := true;
      if debug_load_problem then
	print_problem !problem;
      print_string "problem created\n";
      actions_list := Mylist.empty ();
      init := [];
      goal := [];
    end

  (*****************************************************)
  (************	     Modify Problem       **************)
  (*****************************************************)

  let add_condeff_to_actions map { actions = lactions } = 
    let add_conds { aname = aname; conds = conds } =
      try
	List.iter
	  (fun it -> Mylist.add it conds)
	  (MapString.find aname map)
      with Not_found -> ()
    in Mylist.iter add_conds lactions

  let empty_cond_to_effects { actions = lactions } =
    let up_empty_condeff = function { eff = eff; conds = conds } as ce ->
      begin
	Mylist.filter
	  (fun { cond = cond; ceff = ceff } ->
	     let res = Mylist.is_empty cond 
	     in if res then
		 Mylist.append_to eff ceff;
	       not res)
	  conds;
	let new_eff = SparseSetL.of_mylist eff 
	in ce.eff <- SparseSetL.mylist_of new_eff;
      end
    in Mylist.iter up_empty_condeff lactions

  (*****************************************************)
  (**************  Goals and preconditions   ***********)
  (*****************************************************)

  (* safe for reentrant code. Reset *)
  let goals_and_precs = ref ([]: Lit.t list)
    
  let setup_goals_and_precs () =
    begin
      let tmp_goals_and_precs = ref(SmallSetL.empty) (* safe: reentrant *) in
      List.iter
	(function
	     Clause _ ->
	       raise (Failure "clauses in goals not implemented")
	   | Single l ->
	       tmp_goals_and_precs := SmallSetL.add l !tmp_goals_and_precs)
	(get_goal ());
      Mylist.iter
	(function { prec = p } ->
	   Mylist.iter
	     (fun g -> 
		tmp_goals_and_precs := SmallSetL.add g !tmp_goals_and_precs)
	     p)
	(get_actions ());
      goals_and_precs := SmallSetL.list_of !tmp_goals_and_precs;
      if false then
	begin
	  print_string "Goals and precs:";
	  List.iter Lit.print_lit !goals_and_precs;
	  print_newline();
	end;
    end

  (*****************************************************)
  (**************  Simulate actions	  **************)
  (*****************************************************)

  let print_current_state s =
    begin
      print_string "\n===== ";
      print_set_lit_filtered (fun l -> Lit.lit_is_pos l) s;
      print_string " and ";
      print_set_lit_filtered (fun l -> Lit.lit_is_neg l) s;
    end

  let print_current_state_filtered f s =
    begin
      print_string "\n===== ";
      print_set_lit_filtered (fun l -> Lit.lit_is_pos l && f l) s;
      print_string " and ";
      print_set_lit_filtered (fun l -> Lit.lit_is_neg l && f l) s;
    end

  let update_state state added_set =
    SmallSetL.fold
      (fun elt acc ->
	 SmallSetL.add
	   elt
	   (SmallSetL.remove
	      (Lit.neg_lit elt)
	      acc))
      added_set
      state

  let update_state_ff state added_set =
    let new_added_set = 
      SmallSetL.fold
	(fun elt acc ->
	   if (Lit.lit_is_pos elt &&
		 not(SmallSetL.mem (Lit.neg_lit elt) added_set))
	     || (Lit.lit_is_neg elt) then
	       SmallSetL.add elt acc
	   else acc)
	added_set
	SmallSetL.empty
    in
    SmallSetL.fold
      (fun elt acc ->
	 SmallSetL.add
	   elt
	   (SmallSetL.remove
	      (Lit.neg_lit elt)
	      acc))
      new_added_set
      state
      
  let update_state_delfree state added_set =
    SmallSetL.fold
      (fun elt acc ->
	 SmallSetL.add
	   elt
	   acc)
      added_set
      state
      
  let satisfy_old state p = 
    SmallSetL.mem p state

  let satisfy state p = 
    (SmallSetL.mem p state) ||
      ((Lit.lit_is_neg p) &&
	 (not (SmallSetL.mem (Lit.neg_lit p) state)))

  exception Skip

  let simulate_one do_delfree optimistic state act =
    let simulate_cond added condeffect =
      match condeffect with { cond = cond; ceff = ceff } as w ->
	if !Verbose.level > Verbose.level1 then
	  begin
	    print_string "For condeffects: ";
	    print_cond_eff w;
	  end;
	if Mylist.for_all (satisfy state) cond then
	  begin
	    if !Verbose.level > Verbose.level1 then
	      print_endline "ADDING effect";
            SmallSetL.add_mylist_to ceff added;
	  end
	else
	  begin
	    if !Verbose.level > Verbose.level1 then
	      begin
		print_endline "NOT adding effect because the following does not hold:";
		Mylist.iter (fun c -> if not(satisfy state c) then
			       Lit.print_lit c)
		  cond;
		print_newline();
	      end;
	    added 
	  end in
    let rec simulate_conds added l = 
      Mylist.fold
	(fun acc cond -> (simulate_cond acc cond)) 
	added l in
    let myact =
      try
	Mylist.find
	  (function { aname = a } -> String.uppercase(a) = String.uppercase(act))
	  !problem.actions
      with Not_found ->
	begin
	  print_string "Action: '";
	  print_string act;
	  print_endline "' not found";
	  if optimistic then
	    raise Skip
	  else
	    begin
	      print_string "All we have: ";
	      Mylist.iter (function { aname = a } -> print_endline a) !problem.actions;
	      raise Not_found;
	    end
	end
    in
      begin
	if !Verbose.level > Verbose.level1 then
	  print_endline ("Found action: "^myact.aname);
	if Mylist.for_all (satisfy state) myact.prec then
	  let added =  SmallSetL.add_mylist_to myact.eff SmallSetL.empty in
	    begin
	      if !Verbose.level > Verbose.level1 then
		begin
		  print_string "Adding effects of action:";
		  Mylist.iter Lit.print_lit myact.eff;
		  print_newline();
		end;
	      let res = simulate_conds added myact.conds
	      in if false && do_delfree then (* Delete free, just keep pos *)
		  SmallSetL.filter Lit.lit_is_pos res
		else
		  res
	    end
	else if optimistic then
	  begin
	    print_string "Warning: action ";
	    print_string act;
	    print_endline " not appliable in state. Optimistic execution";
	    print_newline();
	    SmallSetL.empty
	  end
	else
	  begin
	    print_string "Error: action ";
	    print_string act;
	    print_endline " not appliable in state ";
	    print_set_lit state;
	    print_string "precs not found are:";
	    Mylist.iter (fun p -> if not (SmallSetL.mem p state) then Lit.print_lit p) myact.prec;
	    print_newline();
	    raise (Failure "action not appliable in state");
	  end;
      end
	
  let rec simulate_rec do_delfree optimistic state = function
      [] -> state
    | act::rest ->
	if !Verbose.level > Verbose.level1 then
	  print_endline ("to simulate "^act);
	let added = 
	  (try
	     simulate_one do_delfree optimistic state act
	   with
	       Skip -> SmallSetL.empty)
	in begin
	    if not (consistent_set_lit added) then
	      begin
		print_string "Error: resulting effects are not consistent:";
		print_current_state added;
		raise (Failure "resulting effects are not consistent");
	      end;
	    let nstate = (
	      if do_delfree then
		update_state_delfree state added
	      else
		update_state_ff state added)
	    in begin
		print_string "** Simulating: ";
		print_string act;
		print_string "\n\t Changed:";
		if (SmallSetL.exists (fun l -> not (SmallSetL.mem l state)) added ) then
		  begin
		    print_current_state_filtered (fun l -> not (SmallSetL.mem l state)) added;
		    if !Verbose.level > Verbose.level0 then
		      begin
			print_string "\nNew state:";
			print_current_state nstate;
		      end;
		    print_newline ();
		    print_newline ();
		  end
		else
		  print_endline "anything";
		simulate_rec do_delfree optimistic nstate rest;
	      end
	  end

  let report_inconsistent_set_lit s =
    SmallSetL.iter
      (fun elt1 -> 
	 SmallSetL.iter (fun elt2 -> 
			      if elt2 = (Lit.neg_lit elt1) then
				begin
				  print_string "Inconsistency found on lit:";
				  Lit.print_lit elt1;
				  print_newline();
				end)
	   s)
      s 

  (* OJO: probablemente buggy yet. Doesn't work for many relaxed plans... *)
  let simulate do_delfree optimistic fixpoint acts =
    let init_state =
      small_set_lit_from_state !problem.init in
    let rec simulate_aux do_delfree optimistic fixpoint acts init_state =
      begin
	(let all_atoms = get_atoms_set !problem in
	 let natoms = FastSetA.cardinal all_atoms
	 and nlits = SmallSetL.cardinal init_state
	 in if natoms != nlits then
	     begin
	       print_string "WARNING: number of atoms and lits differs: ";
	       print_int natoms;
	       print_string " ";
	       print_int nlits;
	       print_newline ();
	       print_endline "Soundness of simulation can be affected.";
	       if true || natoms > nlits then
		 FastSetA.iter
		   (fun a -> 
		      if not(SmallSetL.mem (Lit.pos_atom a) init_state) &&
			not(SmallSetL.mem (Lit.neg_atom a) init_state) 
		      then
			begin
			  print_string "Atom: ";
			  Lit.print_atom a;
			  print_endline " not in lits"
			end)
		   all_atoms;
	       if false then
		 begin
		   print_endline "ATOMS: "; 
		   FastSetA.iter
		     (fun a -> Lit.print_atom a; print_newline())
		     all_atoms;
		   print_endline "=============================="; 
		   print_endline "ATOMS of LITS: "; 
		   SmallSetL.iter
		     (fun lit -> Lit.print_atom (Lit.atom_of_lit lit); print_newline())
		     init_state;
		   print_endline "==============================";
		 end;
	     end);
	report_inconsistent_set_lit init_state;
	print_string "\n* Init:";
       	print_current_state init_state;
	let final_state = simulate_rec do_delfree optimistic init_state acts 
	in begin
	    print_endline "End of simulation. Final state:";
	    print_string "\n\t Changed from initial state.:";
	    print_current_state_filtered (fun l -> not (SmallSetL.mem l init_state)) final_state;
	    if fixpoint &&
	      (SmallSetL.exists (fun l -> not (SmallSetL.mem l init_state)) final_state) then
		begin
		  print_endline "\n\n**** Simulating again until fixpoint ****\n\n";
		  simulate_aux do_delfree optimistic fixpoint acts final_state
		end
	    else
	      begin
		print_string "\n\t Final state.:";
		print_current_state final_state;
		print_newline ();
		if fixpoint then
		  print_endline "\n\n**** fixpoint reached ****\n\n";
	      end
	  end;
      end 
    in simulate_aux do_delfree optimistic fixpoint acts init_state

  (*****************************************************)
  (**************        Stats             *************)
  (*****************************************************)

  let count { actions = actions } =
    let count_cond (nacts, nconds, ncondeff_lits) { ceff = ceff } =
      (nacts, nconds+1, ncondeff_lits+(Mylist.length ceff)) in
    let count_action (nacts, nconds, ncondeff_lits) { eff = eff; conds = conds } =
      Mylist.fold
	count_cond
	(nacts+1, nconds+1, ncondeff_lits+(Mylist.length eff))
	conds
    in Mylist.fold count_action (0,0,0) actions

  let copy_problem = 
    function { init = linit; goal = lgoal; actions = lactions } as prob ->
      let  copy_lst l = 
	List.map (fun x -> x) l in
      let copy_cond = 
	fun ({ cond = lcond; ceff = lceff } as ce) ->
       	  {ce with cond = Mylist.copy lcond; ceff = Mylist.copy lceff } in
      let copy_actions = 
	fun ({ prec = lprec; eff = leff; conds = lconds } as act) ->
	  { act with prec = Mylist.copy lprec; 
	      eff = Mylist.copy leff; 
	      conds = Mylist.map copy_cond lconds }
      in { prob with init = copy_lst linit;
	     goal = copy_lst lgoal;
	     actions = Mylist.map copy_actions lactions}

  let report_size_problem s { init = linit; goal = lgoal; actions = lactions } =
    let count = ref(0) (* safe: reentrant *) in
    let count_cond ce =
      begin
	count := !count + Mylist.length ce.cond;
	count := !count + Mylist.length ce.ceff;
      end in
    let count_actions act =
      begin
	count := !count + Mylist.length act.prec;
	count := !count + Mylist.length act.eff;
	Mylist.iter count_cond act.conds;
      end
    in begin
	count := !count + List.length linit;
	count := !count + List.length lgoal;
	Mylist.iter count_actions lactions;
	print_string ("Size of " ^ s ^ " problem: ");
	print_int !count;
	print_newline ();
      end

  (*****************************************************)
  (*****   Calculate all models for clauses    *********)
  (*****************************************************)

  let debug_s0_models = false
  let save_cnf_gen is_cnf cnf_nf clauses =
    let str_clauses = ref([]) (* safe: reentrant *) in
    let n_clauses = ref(0) (* safe: reentrant *) in
    let add_to_clause lit =
      let first = 
	if !str_clauses == [] then
	  []
	else
	  List.hd (!str_clauses) in
      let rest = 
	if !str_clauses == [] then
	  []
	else
	  List.tl (!str_clauses) in
      str_clauses := (lit::first)::rest in
    let finish_clause () =
      incr n_clauses;
      str_clauses := []::!str_clauses in
    let make_lit ci atom_to_var next_var =
      let atom = Lit.atom_of_lit ci in
      let sign = if Lit.lit_is_pos ci then 1 else -1
      in try 
	  let v = SmallMapA.find atom atom_to_var in
	  let l = sign*v
	  in l, atom_to_var, next_var
	with Not_found ->
	  let natom_to_var = SmallMapA.add atom next_var atom_to_var in
	  let l = sign*next_var
	  in l, natom_to_var, (next_var+1) in
    let rec clause_to_int next_var atom_to_var = function
	[] -> finish_clause (); next_var, atom_to_var
      | ci::c -> 
	  let lit,natom_to_var,nnext_var = make_lit ci atom_to_var next_var
	  in add_to_clause lit;
	    clause_to_int nnext_var natom_to_var c in
    let rec aux next_var atom_to_var = function
	[] -> next_var, atom_to_var
      | c::lst ->
	  let nnext_var, natom_to_var =
	    clause_to_int next_var atom_to_var c 
	  in aux nnext_var natom_to_var lst in
    let next_var, atom_to_var = aux 1 SmallMapA.empty clauses in
    let cnf = open_out cnf_nf in
      begin
	if debug_s0_models then
	  begin
	    print_endline "Atom to var:";
	    SmallMapA.iter 
	      (fun atom var -> Lit.print_atom atom;
		 print_string ": "; print_int var; print_newline())
	      atom_to_var
	  end;
	if is_cnf then
	  output_string cnf ("p cnf "^(string_of_int (next_var-1))^" "^(string_of_int !n_clauses)^"\n");
	(* output_string cnf !str_clauses; *)
	List.iter
	  (function [] -> ()
	     | lst ->
		 begin 
		   List.iter 
		     (fun i -> output_string cnf (string_of_int i);
			output_string cnf " ") 
		     lst;
		   output_string cnf "0\n";
		 end)
	  !str_clauses;
	if not is_cnf then	
	  output_string cnf "0\n";
	close_out cnf; 
        ((next_var-1), atom_to_var);
      end

  let save_clauses clauses_nf clauses =
    let _, c = save_cnf_gen false clauses_nf clauses
    in c

  let save_cnf cnf_nf clauses =
    save_cnf_gen true cnf_nf clauses
    
  let get_models last_var cnf_nf =
    let out_relsat = Unix.open_process_in ("$TRANSLATOR_HOME/relsat -#a "^cnf_nf) in
    let models = ref([]) (* safe: reentrant *) in
    let get_str_lst line =
      let r1 = Str.regexp ":" in
      let after_colon = (List.hd (List.tl (Str.split r1 line))) in
      let r2 = Str.regexp " "  
      in Str.split r2 after_colon in
    let rec accum n m acc = (* complete with negative vars i: n <= i < m *)
      if n == m then
	acc
      else
	(-(m-1))::(accum n (m-1) acc) in
    let get_a_model last_var line =
      if debug_s0_models then
	begin
	  print_string "line: ";
	  List.iter 	  
	    (fun x -> print_string ("'"^x^"' "))
	    (get_str_lst line);
	  print_newline();
	end;
      let var, last_model =
	List.fold_left 
	  (fun (n,model) lit_txt ->
	     let lit = int_of_string(lit_txt) 
	     in (abs(lit)+1,lit::(accum n (abs lit) model)))
	  (1,[]) (* var-to-explore, model-to-fill *)
	  (get_str_lst line) in
      let model = accum var (last_var+1) last_model 
      in begin
	  if debug_s0_models then
	    begin
	      print_string "var: ";
	      print_int var;
	      print_string "\nlast_var: ";
	      print_int last_var;
	      print_string "\nLast Model: ";
			List.iter (fun i -> print_int i; print_string " ") 
			  (List.rev last_model);
			print_newline ();
	      print_string "Model: ";
	      List.iter (fun i -> print_int i; print_string " ") 
		(List.rev model);
	      print_newline ();
	      print_newline ();
	    end;
	  model;
	end
    in try
	let go = ref(true)  (* safe: reentrant *)
	in while !go do
	    let line = input_line out_relsat 
	    in if string_starts_with line "Solution " then
		models := (get_a_model last_var line)::!models
	      else
		if !models != [] then
		  go := false
	  done;
	  !models
      with a ->
	print_endline ("Error while calling relsat over file "^cnf_nf);
	print_endline "Be sure relsat in $TRANSLATOR_HOME";
	raise a

  let convert_models atom_to_var models =
    let var_to_atom = 
      SmallMapA.fold
	(fun atom var acc ->
	   MapInt.add var atom acc)
	atom_to_var
	MapInt.empty 
    in List.map
	 (fun model -> 
	    List.map
	      (fun nlit ->
		 try 
		   let atom = MapInt.find (abs nlit) var_to_atom 
		   in if nlit > 0 then
		       Lit.pos_atom atom
		     else
		       Lit.neg_atom atom
		 with Not_found ->
		   print_string "Not found var: ";
		   print_int nlit;
		   print_newline();
		   raise (Failure "error in convert_models"))
	      model)
	 models

  let get_models_lits clauses =
    let cnf_nf = ".clauses.cnf" in
    let last_var, atom_to_var = save_cnf cnf_nf clauses in
    let models = get_models last_var cnf_nf 
    in convert_models atom_to_var models 

  (* Take clauses, save them, run pi form, get back.  *)
  let get_in_pi clauses_nf =
    let out_prime_impl = Unix.open_process_in ("(time $TRANSLATOR_HOME/prime-impl < "^clauses_nf^") 2>&1 |tee "^clauses_nf^".pi") in
    let nclauses = ref([]) (* safe: reentrant *) in
    let get_str_lst line =
      let r1 = Str.regexp " \\|[A-Za-z]+" in
      let r2 = Str.regexp ":\\|,"
      in List.map
	   (fun it -> Str.global_replace r1 "" it)
	   (Str.split r2 line) in
    let get_units line =
      List.iter 
	(fun lit_txt ->
	   try
	     let lit = int_of_string(lit_txt) 
	     in nclauses := [lit]::!nclauses
	   with Failure _ -> ())
	(get_str_lst line) in
    let get_a_nclause line =
      if debug_s0_models then
	begin
	  print_string "line: ";
	  List.iter 	  
	    (fun x -> print_string ("'"^x^"' "))
	    (get_str_lst line);
	  print_newline();
	end;
      let last_nclause =
	List.fold_left 
	  (fun nclause lit_txt ->
	     try
	       let lit = int_of_string(lit_txt) 
	       in lit::nclause
	     with Failure _ -> nclause)
	  [] (* nclause-to-fill *)
	  (get_str_lst line) 
      in begin
	  if debug_s0_models then
	    begin
	      print_string "\nLast Nclause: ";
	      List.iter (fun i -> print_int i; print_string " ") 
		(List.rev last_nclause);
	      print_newline ();
	      print_newline ();
	    end;
	  last_nclause;
	end
    in try
	let read = ref(false) in  (* safe: reentrant *)
	let go = ref(true)  (* safe: reentrant *)
	in while !go do
	    let line = input_line out_prime_impl 
	    in if !read then
		begin
		  nclauses := (get_a_nclause line)::!nclauses;
		  if !nclauses == [] then
		    go := false;
		end;
	      if string_starts_with line "Unit:" then
		get_units line;
	      if string_starts_with line "Prime implicates:" then
		read := true;
	      if string_starts_with line "end of prime implicates:" then
		go := false;
	  done;
	  (* last line, report time *)
	  (try
	     let go = ref(true)  (* safe: reentrant *)
	     in while !go do
		 let line = input_line out_prime_impl in
		 let found = 
		   try
		     Str.search_forward (Str.regexp "user") line 0 >= 0
		   with _ -> false
		 in (* print_endline ("LINE"^line); *)
		   if found then
		     begin
		       go := false;
		       Timed.report_time_str "Prime implicates (NOT accumulated. Add to final time)" line;
		     end
	       done
	   with _ -> ());
	  !nclauses
      with a ->
	print_endline ("Error while calling prime-impl over file "^clauses_nf);
	print_endline "Be sure prime-impl in $TRANSLATOR_HOME";
	raise a

  let translate_nclauses nclauses atom_to_var =
    let var_to_atom = 
      SmallMapA.fold
	(fun atom var acc ->
	   MapInt.add var atom acc)
	atom_to_var
	MapInt.empty 
    in List.map
	 (fun nclause -> 
	    List.map
	      (fun nlit ->
		 try 
		   let atom = MapInt.find (abs nlit) var_to_atom 
		   in if nlit > 0 then
		       Lit.pos_atom atom
		     else
		       Lit.neg_atom atom
		 with Not_found ->
		   print_string "Not found var: ";
		   print_int nlit;
		   print_newline();
		   raise (Failure "error in translate_nclauses"))
	      nclause)
	 nclauses

  let get_in_pi_form orig_clauses = 
    let clauses_nf = ".c_i.cls" in
    let atom_to_var = save_clauses clauses_nf orig_clauses in
    let nclauses = List.tl (get_in_pi clauses_nf)
    in translate_nclauses nclauses atom_to_var


  (*****************************************************)
  (**************          Integrity      **************)
  (*****************************************************)

  (* all atoms are mentioned in literals *)
  (* OJO: Not using it. Some other integrity should be made. *)
  let assert_init = function
      { init = linit } as myproblem ->
	let atoms = get_atoms_set myproblem in
	let mentioned_lits = set_from_state_and_clauses linit in
	let mentioned_atoms = set_lit_to_set_atom mentioned_lits in
	  assert( FastSetA.length (FastSetA.union (FastSetA.diff atoms mentioned_atoms)
				       (FastSetA.diff atoms mentioned_atoms))
		  = 0 )
	    
  (* Consistency *)
  (* The consistency of I can be done with code similar to get_the_merge_s0, *)
  (* 	or doing refactoring. If there are no clauses, do a double cycle. *)
  module FastMapInt = FastMap(IntAO)
  module SparseMapInt = SparseMap(IntAO)
  module SmallMapInt = SmallMap(IntAO)

  module PairLits =
  struct
    type t = Lit.t * Lit.t
    let make x1 x2 = 
      if (Lit.hash x2) > (Lit.hash x1) then
	((x1,x2):t)
      else
	((x2,x1):t)
    let compare (x1,x2) (y1,y2) = 
      let one = Lit.compare x1 y1
      in if one <> 0 then
	  one
	else
	  Lit.compare x2 y2
    let equal p1 p2 = (compare p1 p2) == 0
    let hash (x,y) =
      let hx = Lit.hash x in
      let hy = Lit.hash y in
	(hx lsl 7) + hy
      let num _ = raise (Failure "'num' not supported for module PairLits - see source coude")
    let nil = Lit.nil, Lit.nil
  end
  module SetPairLits = SparseSet(PairLits)

  (*****************************************************)
  (**************    Unit propagation     **************)
  (*****************************************************)

  (*
    Unit propagation for "hits*"
    
    - Separate clauses and lits
    - map from lit to value
    - map from lit to watched
    - map from clause to pair of lits
  *)
  module UnitPropL =
  struct
    let debugit = false

    (* all safe for reentrant code. Have reset function *)
    let s_clauses () = Array.make 0 ([]: Lit.t list)
    let clauses = ref( s_clauses() )
    let s_watches () = Array.make 0 (Lit.nil,Lit.nil)
    let watches = ref( s_watches() )
    let s_active () = Array.make 0 true
    let active = ref( s_active() )
    let nclauses = ref 0

    let s_lits_val () = FastSetL.empty ()
    let lits_val = ref(s_lits_val() )
    let s_orig_lits_val () = FastSetL.empty ()
    let orig_lits_val = ref( s_orig_lits_val() )
    let s_orig_init_lits () = []
      (* copy, in list format, of orig_lits_val *)
    let orig_init_lits = ref( s_orig_init_lits() )

    let lit_watch = FastMapL.empty ()
    let lit_desactive = FastMapL.empty ()

    let is_active ci =
      !active.(ci)

    let activate ci =
      !active.(ci) <- true
    let desactivate ci =
      !active.(ci) <- false
	
    let print_all () =
      begin
	print_string "========================================\n";
	let n = ref(0) (* safe: reentrant *) in
	  print_string "\nClauses (";
	  print_int !nclauses;
	  print_string "): \n";
	  Array.iter
	    (fun c ->
	       print_int !n;
	       print_string "(";
	       if is_active !n then
		 print_string "active"
	       else
		 print_string "not active";
	       print_string"): ";
	       incr n;
	       print_list_lit c)
	    !clauses;
	  n := 0;
	  print_string "\nWatches:\n";
	  Array.iter
	    (function p,q ->
	       begin
		 print_int !n;
		 print_string ":";
		 incr n;
		 Lit.print_lit p;
		 print_string " ,";
		 Lit.print_lit q;
		 print_string "\n";
	       end)
	    !watches;
	  print_string "\nLits val: ";
	  print_fastset_lit !lits_val;
	  (*       FastSetL.iter (fun p _ -> Lit.print_lit p; print_string ", ") !lits_val; *)
	  print_string "\nOrig Lits val: ";
	  print_fastset_lit !orig_lits_val;
	  (*       FastSetL.iter (fun p _ -> Lit.print_lit p; print_string ", ") !orig_lits_val; *)
	  print_string "\nlit_watch:\n";
	  FastMapL.iter
	    (fun p l ->
	       begin
		 Lit.print_lit p;
		 print_string " -> ";
		 SmallSetInt.iter (fun n -> print_int n; print_string ", ") l;
		 print_string "\n";
	       end
	    )
	    lit_watch;
	  print_string "========================================\n";
	  flush stdout;
      end
	
    let make_lit_true l =
      begin
	(try
	   let nclauses = FastMapL.find lit_desactive l
	   in SmallSetInt.iter (fun ci -> desactivate ci) nclauses
	 with Not_found -> ());
	FastSetL.add l !lits_val;
      end

    let is_lit_true l =
      FastSetL.mem !lits_val l

    let separate init =
      let count_clauses = ref(0) (* safe: reentrant *)
      in begin
	  List.iter
	    (function
		 Clause _ -> incr count_clauses
	       | Single l -> make_lit_true l)
	    init;
	  clauses := Array.make !count_clauses [];
	  watches := Array.make !count_clauses (Lit.nil,Lit.nil);
	  active := Array.make !count_clauses true;
	end
	   
    let s_set_clauses () = SetSetL.empty
    let set_clauses = ref( s_set_clauses () )
    let set_added_item c = 
      let s = SmallSetL.of_list c
      in set_clauses := SetSetL.add s !set_clauses
    let added_item c = 
      let s = SmallSetL.of_list c
      in SetSetL.mem s !set_clauses
	   
    let add_item = function
	Clause [] -> ()
      | Single a | Clause [a] ->
	  make_lit_true a
      | Clause ((c1::c2::_ ) as c) ->
	  if false then
	    begin
	      print_string "*!*!*! Adding clause: ";
	      print_list_lit c;
	    end;
	  if not (added_item c) then
	    let nc1 = Lit.neg_lit c1
	    and nc2 = Lit.neg_lit c2
	    in begin
		set_added_item c;
		!clauses.(!nclauses) <- c;
		!watches.(!nclauses) <- (nc1,nc2);
		fast_set_lit_add_to_ints lit_watch nc1 !nclauses;
		fast_set_lit_add_to_ints lit_watch nc2 !nclauses ;
		List.iter
		  (fun ci -> fast_set_lit_add_to_ints lit_desactive ci !nclauses )
		  c;
		incr nclauses;
	      end
	  else if false then
	    begin
	      print_string "*!*!*! This clause was already added: ";
	      print_list_lit c;
	    end
	      

    let put_clauses init =
      List.iter add_item init

    exception Found of Lit.t
    exception Satisfied
      (* Try to find other support
	 need to save new lits in hash to recover them
	 later on can overwrite old table.
	 other option: have two tables.
	 bad: create tables eachtime...

	 Invariant: l is one of the watches of clause cindex:
	 l \in !watches.(cindex)
      *)
    let trigger l cindex acclits =
      if is_active cindex then
	let clause = !clauses.(cindex) in
	let (first,second) = !watches.(cindex) in
	let (bad,ok) =
	  assert( first == l or second == l );
	  if first == l then
	    (first, second)
	  else
	    (second, first) in
	let find_watch ci =
	  let neg_ci = Lit.neg_lit ci
	  in if (neg_ci != bad && neg_ci != ok ) then
	      begin
		if is_lit_true ci then
		  raise Satisfied
		else if not (is_lit_true neg_ci) then
		  raise (Found neg_ci)
	      end
	in try
	    begin
	      List.iter find_watch clause;
	      (* postcond: all literals not in {bad, ok} are false
		 so: trigger literal "ok"
	      *)
	      desactivate cindex; (* anyway *)
	      if debugit then
		begin
		  print_string "About lit true:";
		  Lit.print_lit ok;
		end;
	      if is_lit_true ok then
		begin
		  if debugit then
		    begin
		      print_string ": was true\n";
		      flush stdout;
		    end;
		  acclits
		end
	      else if is_lit_true (Lit.neg_lit ok) then
		begin
		  if debugit then
		    begin
		      print_string ": was false\n";
		      flush stdout;
		    end;
		  raise (Failure "contradiction detected on init")
		end
	      else
		begin
		  if debugit then
		    begin
		      print_string ": making lit true\n";
		      flush stdout;
		    end;
		  make_lit_true (Lit.neg_lit ok);
		  (Lit.neg_lit ok)::acclits;
		end
	    end
	  with
	      Satisfied -> desactivate cindex; acclits
	    | Found to_watch ->
		begin
		  if debugit then
		    begin
		      print_string "saving watch for clause: ";
		      print_int cindex;
		      print_string ". watch is:";
		      Lit.print_lit to_watch;
		      print_string "\n";
		    end;
                  !watches.(cindex) <- (ok,to_watch);
		  fast_set_lit_add_to_ints lit_watch to_watch cindex;
		  fast_set_lit_remove_from_ints lit_watch bad cindex;
		  acclits;
		end
      else
	acclits
	  
    (*
      Until fixpoint
      for each clausula C,
      -- let d = C - neg_lits
      -- if size(d) = 0, fail
      -- if size(d) = 1
      ----- neg_lits += neg(lit in d)
      ----- quitar c de clausulas activas
    *)

    (* take recently added literals
       should be added FIRST to lits_val
    *)
    let rec trigger_lits acc = function
	[] -> acc
      | l::lits ->
	  try
	    let to_trigger = FastMapL.find lit_watch l
	    in trigger_lits (l::acc) (SmallSetInt.fold (trigger l) to_trigger lits)
	  with Not_found ->
	    trigger_lits (l::acc) lits
	      
    let get_list_vals () =
      FastSetL.list_of !lits_val
	
    let debug_unit_prop = false
    let do_unit_prop l =
      begin
	if debug_unit_prop or debugit then
	  begin
	    print_endline "--------------------";
	    print_string "Pre Table:";
	    print_all();
	    print_string "*** adding lits:";
	    List.iter Lit.print_lit l;
	    print_string "\n";
	    flush stdout;
	  end;
(* 	List.iter make_lit_true l; *)
(* 	let nlits = trigger_lits [] l in   *)

	let nlits =
	  List.fold_left 
	    (fun nlits lit ->
	       begin
		 make_lit_true lit;
		 trigger_lits nlits [lit];
	       end
	       )
	    []
	    l in

	  if debug_unit_prop or debugit then
	    begin
	      print_string "Obtained:";
	      List.iter Lit.print_lit nlits;
	      print_endline "\n--------------------";
	      if debugit then
		begin
		  print_string "Table:";
		  print_all();
		  print_endline "--------------------";
		end
	    end;
	  
	  (* Restart *)
	  lits_val := FastSetL.copy !orig_lits_val;
	  for n = 0 to !nclauses - 1 do
	    activate n;
	  done;
	  
	  (* Result*)
	  nlits
      end

    let s_cache_unit_prop_new () = MapSetL.empty
    let cache_unit_prop_new = ref( s_cache_unit_prop_new () )
    let unit_prop_new l =
      let s = (SmallSetL.of_list l)
      in try
	  MapSetL.find s !cache_unit_prop_new
	with Not_found ->
	  let res = SmallSetL.of_list (do_unit_prop l)
	  in cache_unit_prop_new := MapSetL.add s res !cache_unit_prop_new;
	    res

    let s_cache_hits_star () = MapSetL.empty
    let cache_hits_star = ref( s_cache_hits_star () )
    let hits_star l =
      let s = (SmallSetL.of_list l)
      in try
	  MapSetL.find s !cache_hits_star
	with Not_found ->
	  (* let res1 = unit_prop_new l in *)
(* 	  let res1 = SmallSetL.of_list (do_unit_prop l) in *)
(* 	  let res = SmallSetL.add_list_to !orig_init_lits res1 in (\*OJO OJO: GO BACK! caching non effective *\) *)
	  let res = SmallSetL.of_list ((do_unit_prop l)@ !orig_init_lits)
	  in cache_hits_star := MapSetL.add s res !cache_hits_star;
	    res


    (* reset all module data structures *)
    let reset_unit_prop () =
      begin
	clauses := s_clauses ();
	watches := s_watches ();
	active := s_active ();
	nclauses := 0;
	lits_val := s_lits_val ();
	orig_lits_val := s_orig_lits_val ();
	orig_init_lits := s_orig_init_lits ();
	cache_hits_star := s_cache_hits_star ();
	set_clauses := s_set_clauses ();
      end

    let setup_unit_prop { init = init } =
      begin
	reset_unit_prop();

	separate init;
	put_clauses init;
	orig_init_lits := trigger_lits [] (get_list_vals ());
	orig_lits_val := FastSetL.copy !lits_val;
	if debugit then
	  print_all ();
      end

  end


  (*****************************************************)
  (**************          Mutex Set      **************)
  (*****************************************************)
    
  (* let debug_transaction_mutex = false *)
  (* let mutex_natoms = ref(0) *)
  module MutexC = FastSetL
  (* module MutexC = SparseSetL *)
  let mutex_with = FastMapL.empty() (* safe for reentry. reset in reset() *)
  let mutex_with_l l =
    try
      FastMapL.find mutex_with l
    with Not_found -> 
      let mutex_with_l = MutexC.empty()
      in (* FastSetL.fill mutex_with_l !mutex_natoms; *)
	FastMapL.add l mutex_with_l mutex_with;
	mutex_with_l
  exception Mutex
  let mutex_set_do = function { init = linit; actions = lactions; goal = lgoal } as problem ->
    let debug_transaction_mutex = !Verbose.level > Verbose.level1 in
    let debug_transaction_mutex2 = false in
    let make_mutex_with l l2 =
      MutexC.add l2 (mutex_with_l l) in
    let make_no_mutex_with l l2 =
      MutexC.remove l2 (mutex_with_l l) in
    let mutex = ref(SetPairLits.empty()) in (* safe for reentry. reset in reset() *)
    let atoms = get_atoms_set problem in
    let reset () =
      begin
	(* mutex_natoms := FastSetA.cardinal atoms;  *)
	mutex := SetPairLits.empty();
	FastMapL.clear mutex_with;
	(* FastMapL.fill mutex_with !mutex_natoms; *)
      end in
    let is_mutex_pair x1 x2 =
      (* tambien posible desde mutex_with *)
      let mx1 = mutex_with_l x1 
      in MutexC.mem mx1 x2 in

    let make_mutex p1 p2 =
      if p1 != p2 && not(is_mutex_pair p1 p2) then
	begin
	  make_mutex_with p1 p2;
	  make_mutex_with p2 p1;
	  if false then
	    begin
	      print_string "New mutex: ";
	      Lit.print_lit p1;
	      Lit.print_lit p2;
	      print_newline();
	    end
	end in

    (* until fixpoint  *)
    let debug_non_mutex = ref(true) in
    let changed = ref(false) in
    let non_mutex x1 x2 =
      if x1 != x2 then
	begin
	  if !debug_non_mutex or debug_transaction_mutex then
	    begin
	      (if is_mutex_pair x1 x2 then
		 print_string "Not mutex anymore: "
	       else
		 print_string "Was not mutex anyway: ");
	      Lit.print_lit x1;
	      Lit.print_lit x2;
	      print_newline();
	    end;
	  if is_mutex_pair x1 x2 then
	    changed := true;
	  make_no_mutex_with x1 x2;
	  make_no_mutex_with x2 x1;
	  (* SetPairLits.remove (PairLits.make x1 x2) !mutex  *)
	end in
      
    let non_mutex_pair_of_lst lst =
      let rec aux2 l = function
	  [] -> ()
	| l2::lst ->
	    non_mutex l l2; aux2 l lst in
      let rec aux = function
	  [] -> ()
	| l::lst ->
	    aux2 l lst; aux lst
      in aux lst in
      
(*     let neg_atoms_not_mentioned () =  *)
(* 	(\* Non mentioned atoms, assume false *\) *)
(*       let init_from_linit = set_from_state_and_clauses linit in *)
(*       let atoms_from_linit = set_lit_to_set_atom init_from_linit in *)
(*       let not_mentioned_from_linit = FastSetA.diff atoms atoms_from_linit  *)
(*       in neg_set_atoms not_mentioned_from_linit in *)

    let debug_clean = false in

    (* all possible L,L' in state, is not mutex if L != -L' *)
    let clean_for_init_new () =
      let remove_conseq_of lit =
	let lit_star = UnitPropL.hits_star [lit]
	in begin
	    if debug_clean then
	      begin
		print_string "From lit";
		Lit.print_lit lit;
		print_string " got: ";
		print_set_lit lit_star;
		print_newline();
	      end;
	    SmallSetL.iter
	      (fun litp -> make_mutex lit (Lit.neg_lit litp))
	      lit_star;
	  end
      in begin	  
	  if debug_clean then
	    begin
	      print_endline "NEW!!!";
	      print_state linit;
	      print_newline();
	    end;
	  FastSetA.iter
	    (fun a -> 
	       if debug_clean then
		 begin
		   print_string "Seeing atom for consequen: ";
		   Lit.print_atom a;
		   print_newline();
		 end;
	       remove_conseq_of (Lit.pos_atom a);
	       remove_conseq_of (Lit.neg_atom a))
	    atoms;
	  debug_non_mutex := false;
	end in

    let clean_for_init_new3 () =
      let init_lits = UnitPropL.hits_star [] in
      let init_atoms = 
	SmallSetL.fold
	  (fun l acc -> FastSetA.add (Lit.atom_of_lit l) acc; acc )
	  init_lits
	  (FastSetA.empty ()) in
      let ninit_lits =
	SmallSetL.fold
          (fun lit acc -> (Lit.neg_lit lit)::acc)
	  init_lits
	  [] in
      let trivial_mutex lit =
	let nlit = Lit.neg_lit lit
	in List.iter 
	     (* (fun l -> ()) *)
	     (make_mutex lit)
	     ninit_lits in
      let remove_conseq_of lit =
	let conseq = UnitPropL.unit_prop_new [lit]
	in begin
	    if debug_clean then
	      begin
		print_string "From lit";
		Lit.print_lit lit;
		print_string " got: ";
		print_set_lit conseq;
		print_newline();
	      end;
	    trivial_mutex lit;
	    SmallSetL.iter
	      (* (fun litp -> let nl = Lit.neg_lit litp in ()) *)
	      (fun litp -> make_mutex lit (Lit.neg_lit litp))
	      conseq;
	  end in
      let proc_atom a =
	if FastSetA.mem init_atoms a then
	  begin
	    trivial_mutex (Lit.pos_atom a);
	    trivial_mutex (Lit.neg_atom a);
	  end
	else
	  begin
	    remove_conseq_of (Lit.pos_atom a);
	    remove_conseq_of (Lit.neg_atom a)
	  end
      in begin	  
	  if debug_clean then
	    begin
	      print_endline "NEW!!!";
	      print_state linit;
	      print_newline();
	    end;
	  FastSetA.iter
	    (fun a -> 
	       if debug_clean then
		 begin
		   print_string "Seeing atom for consequen: ";
		   Lit.print_atom a;
		   print_newline();
		 end;
	       proc_atom a)
	    atoms;
	  debug_non_mutex := false;
	end in


    (* last index of the 1st element *)
    let last_i_1st lst_mylst = 
      (Mylist.length (List.hd lst_mylst)) - 1 in

    (* verify lit against each mylist in lst,
       starting from index i in (hd lst), backwards *)
    let is_mutex_respect_to_mylst_aux lit i lst =
      let rec aux i = function 
	  [] -> raise (Failure "wild code in is_mutex_respect_to_mylst_aux ")
	| (ml::lst) as l ->
	    if i < 0 then (* done with ml *)
	      (if lst == [] then
		 false
	       else
		 aux (last_i_1st lst) lst)
	    else
	      if is_mutex_pair lit (Mylist.get ml i) then
		true
	      else aux (i-1) l
      in aux i lst in
      
    (* there is mutex between l and any in list of Mylist  *)
    let is_mutex_respect_to_lst_mylst l lst_mylst = 
      let res = 
	is_mutex_respect_to_mylst_aux 
	  l (last_i_1st lst_mylst) lst_mylst
      in begin
	  if debug_transaction_mutex2 then
	    begin
	      print_endline "-------------------";
	      print_string "Literal";
	      Lit.print_lit l;
	      if res then
		print_string " is mutex respect to "
	      else
		print_string " is not mutex respect to ";
	      List.iter print_mylist_lit lst_mylst;
	      print_endline "-------------------";
	    end;
	  res;
	end in

    (* there is mutex L.L' in some combination in list of Mylist  *)
    let is_mutex_lst_mylst lst_mylst =
      assert(lst_mylst != []);
      let rec aux i = function 
	  [] -> raise (Failure "wild code in is_mutex_lst_mylst")
	| (ml::lst) as l ->
	    if i < 0 then (* done with ml *)
	      (if lst == [] then
		 false
	       else
		 aux (last_i_1st lst) lst)
	    else
	      if is_mutex_respect_to_mylst_aux (Mylist.get ml i) (i-1) l then 
		true
	      else aux (i-1) l in
      let res = aux (last_i_1st lst_mylst) lst_mylst
      in begin
	  if debug_transaction_mutex then
	    begin
	      print_endline "-----------";
	      if res then
		print_string "Mutex: "
	      else
		print_string "Non mutex: ";
	      List.iter print_mylist_lit lst_mylst;
	      print_endline "-----------";
	    end;
	  res;
	end in      
      
    let active_actions = ref([]) in (* OJO: NOT safe yet *)
    let actions2active_conds = FastMapInt.empty() in (* OJO: NOT safe yet *)
    (* In principle, all actions are active *)
    let fill_active () = 
      let fill_conds conds = 
	let rec till_0 acc i = 
	  if i < 0 then (-1::acc) (* -1 represents the unconditioanl effect of action *)
	  else till_0 (i::acc) (i-1) 
	in till_0 [] ((Mylist.length conds)-1) in
      let rec fill_actions i =
	if i < 0 then ()
	else
	  let act_conds = fill_conds ((Mylist.get lactions i).conds)
	  in begin
	      FastMapInt.add i act_conds actions2active_conds;
	      active_actions := i::!active_actions;
	      fill_actions (i-1);
	    end
      in begin
	  active_actions := [];
	  fill_actions ((Mylist.length lactions)-1);
	  if false then
	    begin
	      print_endline "Active actions and conds: ";
	      List.iter
		(fun act ->
		   print_int act;
		   print_string ": ";
		   print_list_int (FastMapInt.find actions2active_conds act))
		!active_actions;
	      print_endline "--------------------";
	    end;
	end in
      
    let act_i i =
      Mylist.get lactions i in
    let prec_act_i i = 
      (act_i i).prec in
    let mylist_nil = Mylist.empty() in
    let condeff_i_j act cond =
      if cond == -1 then
	{ cond = mylist_nil; ceff = (act_i act).eff; ccomment = "uncond effect" }
      else
	(Mylist.get (act_i act).conds cond) in
    let cond_i_j act cond =
      (condeff_i_j act cond).cond in
    let effect_i_j act cond = 
      (condeff_i_j act cond).ceff in
      
    let clause_one_conds act real_active_conds =
      (* do not consider the same rule, as it was desactivate if contradiction? *)
      (* for each pair of active rules *)
      let see_pair_cond cond_i cond_j =
	if not(is_mutex_lst_mylst [prec_act_i act;cond_i_j act cond_i;cond_i_j act cond_j]) then
	  begin
	    if debug_transaction_mutex then
	      begin
		print_endline "..............................";
		print_endline "this two cond-eff are non-mutex:";
		print_cond_eff (condeff_i_j act cond_i);
		print_cond_eff (condeff_i_j act cond_j);
		print_endline "..............................";
	      end;
	    (* prec(a)+cond_i+cond_j is not mutex, then any comb of effects is non mutex *)
	    Mylist.iter_pair2
	      non_mutex 
	      (effect_i_j act cond_i)
	      (effect_i_j act cond_j);
	  end
      in list_iter_pair_same see_pair_cond real_active_conds in

    let mutex_implies_calc lst11 lst12 lst2 =
      let s1 = SmallSetL.add_mylist_to lst11 (SmallSetL.add_mylist_to lst12 SmallSetL.empty) in
      let s2 = SmallSetL.add_mylist_to lst2 SmallSetL.empty in
      let s = SmallSetL.diff s2 s1
      in SmallSetL.for_all
	   (fun l -> is_mutex_respect_to_lst_mylst (Lit.neg_lit l) [lst11;lst12])
	   s in 
    let mutex_implies act cond_i cond_j =
      (* OJO: use cache *)
      let res = mutex_implies_calc (prec_act_i act) (cond_i_j act cond_i) (cond_i_j act cond_j) 
      in begin
	  if debug_transaction_mutex then
	    begin
	      print_endline "..............................";
	      print_cond_eff (condeff_i_j act cond_i);
	      if res then
		print_endline "implies"
	      else
		print_endline "NO implies";
	      print_cond_eff (condeff_i_j act cond_j);
	      print_endline "..............................";	      
	    end;
	  res
	end in
    let clause_two_conds act real_active_conds =
      (* do not consider the same rule, as it was desactivate if contradiction? *)
      (* for each pair of active rules *)
      let see_cond cond_i =
	let see_effect l =
	  let nl = Lit.neg_lit l in
	  let mutex_with_l = mutex_with_l l in
	  let non_saved = MutexC.copy mutex_with_l in
	  let save l = MutexC.remove l non_saved in

	  let save_with_three () =
	    let each_rule cond_j =
              if mutex_implies act cond_i cond_j then
		(* save because of rule 3.c:
		   if c_i => c_j then l,-l2 is ok as mutex (save) *)
		  Mylist.iter (fun l2 -> save (Lit.neg_lit l2)) (effect_i_j act cond_j)
	    in List.iter each_rule real_active_conds in

	  let save_with_two () =
	    let each_x l2 =
	      if is_mutex_respect_to_lst_mylst
		l2 [prec_act_i act;cond_i_j act cond_i] then
		  (* if mutex(prec(a)+cond_i+l2), then mutex is ok, save it *)
		  save l2
	    in MutexC.iter each_x mutex_with_l 
		 
	  in begin
	      save nl;
	      save_with_two ();
	      save_with_three ();
	      MutexC.iter (non_mutex l) non_saved;
	    end
	in Mylist.iter see_effect (effect_i_j act cond_i)
      in List.iter see_cond real_active_conds in

    let clauses_conds act =
      let active_conds = FastMapInt.find actions2active_conds act in
      let real_active =
	List.filter 
	  (fun _ -> true)
	      (* not(is_mutex_lst_mylst [prec_act_i act;cond_i_j act j])) *)
	  active_conds 
      in begin
	  clause_one_conds act real_active;
	  clause_two_conds act real_active;
	  (* FastMapInt.add act real_active actions2active_conds; *)
	end in
     
    let verif_mutex () =
      let ok = ref(true) in
      let verif_two act conds =
	let some_eff_mutex cond_i cond_j =
	  Mylist.exists
	    (fun li ->
	       Mylist.exists
		 (fun lj -> is_mutex_pair li lj)
		 (effect_i_j act cond_j))
	    (effect_i_j act cond_i) in
	let see_cond_i cond_i =
	  let see_cond_j cond_j =
	    if (some_eff_mutex cond_i cond_j)
	      && not(is_mutex_lst_mylst [prec_act_i act;cond_i_j act cond_i;cond_i_j act cond_j]) then
		begin
		  ok := false;
		  print_endline "------------------------------";
		  print_endline "Mutex def failed (2):";
		  print_cond_eff (condeff_i_j act cond_i);
		  print_cond_eff (condeff_i_j act cond_j);
		  print_endline "------------------------------";		  
		end
	  in List.iter see_cond_j conds
	in List.iter see_cond_i conds in
      let verif_three act conds =
	let see_cond cond_i =
	  let see_effect l =
	    let nl = Lit.neg_lit l in
	    let mutex_with_l = mutex_with_l l in
	    let non_saved = MutexC.copy mutex_with_l in
	    let save l = MutexC.remove l non_saved in	      
	    let save_with_rules () =
	      let each_rule cond_j =
		if mutex_implies act cond_i cond_j then
		  (* save because of rule 3.c:
		     if c_i => c_j then l,-l2 is ok as mutex (save) *)
		  Mylist.iter (fun l2 -> save (Lit.neg_lit l2)) (effect_i_j act cond_j)
	      in List.iter each_rule conds in
	    let save_with_mutex () =
	      let each_x l2 =
		let lst = Mylist.empty() in
		  Mylist.add l2 lst;
		  if is_mutex_lst_mylst [lst;prec_act_i act;cond_i_j act cond_i] then
		    save l2
	      in MutexC.iter each_x mutex_with_l 
	    in begin
		save nl;
		save_with_rules ();
		save_with_mutex ();
		if MutexC.cardinal non_saved <> 0 then
		  begin
		    ok := false;
		    print_endline "------------------------------";
		    print_endline "Mutex def failed (3):";		    
		    print_endline "The following lits are mutex with reason:";
		    (* print_fastset_lit non_saved; *)
		    print_endline "------------------------------";		  
		  end
	      end
	  in Mylist.iter see_effect (effect_i_j act cond_i)
	in List.iter see_cond conds in
      let verif_conds act = 
	let conds = FastMapInt.find actions2active_conds act in
	  begin
	    verif_two act conds;
	    verif_three act conds;
	    (* FastMapInt.add act real_active actions2active_conds; *)
	  end in
      let rec aux = function
	  [] -> ()
	| i::lst -> 
	    begin
	      verif_conds i;
	      aux lst;
	    end
      in begin
	  aux !active_actions;
      	  if !ok then
	    print_endline "Mutex definition verification: ok"
	  else
	    print_endline "Mutex definition verification: FAIL"
	end in
      
    let update_clauses () =
      let rec aux new_active = function
	  [] -> () (* active_actions := new_active *)
	| i::lst -> 
            if false (* What's the condition???? BUGGY is_mutex_lst_mylst [prec_act_i i] *) then
	      aux new_active lst
	    else 
	      begin
		if debug_transaction_mutex then
		  print_endline ("Seeing action "^((act_i i).aname));
		clauses_conds i;
		aux (i::new_active) lst;
	      end
      in aux [] !active_actions in
    let niter = ref(0) in (* OJO: NOT safe *)
    let rec fixpoint () =
      begin
	if false then
	  begin
	    print_string "********** Iteration num ";
	    print_int !niter;
	    print_endline " of fixpoint";
	    incr niter;
	  end;
	changed := false;
	update_clauses ();
	if !changed then
	  fixpoint ();
      end in
    let print_mutex () =
      let print_mutex_with l =
	let mutex_with_l = mutex_with_l l in
	let s = MutexC.fold
	  (fun i acc -> SmallSetL.add i acc)
	  mutex_with_l
	  SmallSetL.empty
	in begin
	    print_string "\nlit";
	    Lit.print_lit l;
	    print_string " is mutex with ";
	    (* print_fastset_lit mutex_with_l; *)
	    print_set_lit s;
	  end
      in begin
	print_endline "==================================";
	print_endline "Mutex Table:\n";
	FastSetA.iter
	  (fun a -> print_mutex_with (Lit.pos_atom a); print_mutex_with (Lit.neg_atom a))
	  atoms;
	print_endline "==================================";
	end in
    let print_results_consistent () =
      begin
	let print_header = ref(false) in
	let consistent = ref(true)
	in FastSetA.iter
	     (fun a -> 
	        if not(is_mutex_pair (Lit.pos_atom a) (Lit.neg_atom a)) then
		  begin
		    if !print_header then
		      begin
			print_endline "****************************************";
			print_endline "Consistency";
			print_header := false;
		      end;
		    print_string "Mutex no valido on atom: ";
		    Lit.print_atom a;
		    print_newline ();
		    consistent := false;
		  end)
	     atoms;
	  if !consistent then
	    print_endline "Problem is consistent: mutex L,-L for all L"
	  else
	    begin
	      print_endline "Problem is NOT consistent: for some L, there is no mutex L,-L";
	      print_endline "****************************************";
	    end
      end in
    let print_results_consistent_goal () =
      begin
	let print_header = ref(false) in
	let consistent = ref(true)
	in list_iter_pair_same
	     (fun l1 l2 ->
	        if is_mutex_pair l1 l2 then
		  begin
		     if !print_header then
		       begin
			 print_endline "****************************************";
			 print_endline "Consistency of GOAL";
			 print_header := false;
		       end;
		    print_string "Mutex entre GOAL lits ";
		    Lit.print_lit l1;
		    Lit.print_lit l2;
		    print_newline ();
		    consistent := false;
		  end)
	     (list_lit_from_state lgoal);
	  if not !consistent then
	    begin
	      print_endline "goal NOT consistent!";
	      print_endline "the problem is inconsistent, the goal is unreachable";
	      print_endline "or there is a bug in mutex verification";
	      print_endline "****************************************";
	    end
      end
    in begin
	reset();
	print_endline "\nVerifying consistency by mutexset";
	if false then
	  Timed.call "clean_for_init_new"
	    (fun () ->
	       clean_for_init_new ())
	else
	  Timed.call "clean_for_init_new3"
	    (fun () ->
	       clean_for_init_new3 ());
	(* exit(1); *)
	fill_active ();
	Timed.call "fixpoint"
	  (fun () ->
	     fixpoint());
	(* verif_mutex(); *)
	if !Verbose.level > Verbose.level1 then
	  print_mutex ();
	print_results_consistent();
	if false then
	  begin
	    print_results_consistent_goal();
	  end;
	(* exit(0); *)
      end
  let mutex_set use_short_names p =
    begin
      if use_short_names && !Verbose.level > Verbose.level1 then
	use_now_short_names := true;
      (try 
	 mutex_set_do p
       with _ -> print_endline ("Failure while verifying consistency"));
      use_now_short_names := false
    end
end

module StringAtom =
struct
  type t = string
  let compare = compare
  let equal x y = x = y
  let hash = Hashtbl.hash
  let to_text = fun x -> x
  let nil = ""
end

module GProblem = Problem(StringAtom)

(*****************************************************)
(************   Normalize PDDL        ****************)
(*****************************************************)

module Normalize =
struct
  module GProblemNew = Problem(StringAtom)

  let load_atoms_and_lits () = 
    let s = GProblem.get_atoms_set (!GProblem.problem)
    in GProblem.FastSetA.iter
	 (fun a -> 
	    let na = GProblemNew.Lit.make_atom(GProblem.Lit.A.get a)
	    in begin
		ignore(GProblemNew.Lit.neg_atom na);
		ignore(GProblemNew.Lit.pos_atom na)
	      end)
	 s
	 
  let copy_lit lit =
    let atom = GProblemNew.Lit.make_atom(GProblem.Lit.A.get (GProblem.Lit.atom_of_lit lit))
    in if GProblem.Lit.lit_is_pos lit then
	GProblemNew.Lit.calc_pos_atom atom
      else
	GProblemNew.Lit.calc_neg_atom atom
  let copy_lst_lit lst =
    let temps = ref(GProblemNew.SmallSetL.empty) (* safe: reentrant *)
    in Mylist.iter 
	 (fun lit -> temps := GProblemNew.SmallSetL.add (copy_lit lit) !temps)
	 lst;
      GProblemNew.SmallSetL.mylist_of !temps

  let copy_cond { GProblem.ccomment = ccomment;
		   cond = cond; ceff = ceff } =
    { GProblemNew.ccomment = ccomment;
      cond = copy_lst_lit cond;
      ceff = copy_lst_lit ceff }
  let copy_action { GProblem.aname = aname; prec = prec; 
		    eff = eff; conds = conds } =
    Mylist.add { GProblemNew.aname = aname;
		 prec = copy_lst_lit prec;
		 eff = copy_lst_lit eff;
		 conds = Mylist.map copy_cond conds }
      (GProblemNew.get_actions())
  let load_actions () =
    let acts = ref(MapString.empty) (* safe: reentrant *)
    in begin
	Mylist.iter 
	  (function { GProblem.aname = aname} as act ->
	     acts := MapString.add aname act !acts)
	  (GProblem.get_actions ());
	MapString.iter
	  (fun _ act -> copy_action act)
	  !acts;
      end
  let trans_state_item = function
      GProblem.Single lit -> GProblemNew.Single (copy_lit lit)
    | GProblem.Clause lst -> GProblemNew.Clause (List.map copy_lit lst)
  let load_problem () =
    begin
      GProblemNew.set_instance (!GProblemNew.problem) (GProblem.get_instance());
      GProblemNew.set_domain (!GProblemNew.problem) (GProblem.get_domain());
      GProblemNew.set_init (!GProblemNew.problem) (List.map trans_state_item (GProblem.get_init()));
      GProblemNew.set_goal (!GProblemNew.problem) (List.map trans_state_item (GProblem.get_goal()));
    end
  let normalize () =
    begin
      Timed.call "NORMALIZE"
	(fun () ->
	   begin
	     print_endline "Starting to normalize...";
	     Timed.call "load atoms and lits" load_atoms_and_lits;
	     Timed.call "load_actions" load_actions;
	     Timed.call "load_problem" load_problem;
	     Timed.call "compact conds"
	       (fun () -> GProblemNew.problem := GProblemNew.compact_conds !GProblemNew.problem);
	     Timed.call "saved pddl"
	       (fun () -> GProblemNew.print_pddl "normalized")
	   end);
      print_endline "normalized PDDL has been saved in normalized-{d,p}.pddl";
    end
end

(*****************************************************)
(************   Copy acts of non-determinism   *******)
(*****************************************************)

(* Assumes that non-deterministics effects (oneof e1 ... en)
were converted by introducing in init: (oneof h1 ... hn)
and conditional effects: h_i -> e_i *)
module Nondeterminism =
struct
  let magic_token = "ONEOF----"

  let re = Str.regexp_case_fold magic_token
  let coincides_txt txt =
    try
      ignore(Str.search_forward re txt 0);
      true
    with Not_found -> false
  let coincides l =
    coincides_txt (GProblem.Lit.A.to_text (GProblem.Lit.atom_of_lit l))

  exception IsDeterministic
  let collect_inits linit =
    let collect_one acc = function
	GProblem.Single _ -> acc
      | GProblem.Clause l -> 
	  if List.for_all coincides l then
	    (GProblem.SmallSetL.of_list l)::acc
	  else
	    begin
	      assert(not(List.exists coincides l));
	      acc;
	    end in
    let res = List.fold_left collect_one [] linit 
    in begin
	if res == [] then
	  raise IsDeterministic
	else
	  res
      end

  let non_det_acts = ref(SetString.empty)
  let add_non_det_acts { GProblem.aname = aname} =
    if false && not (SetString.mem aname !non_det_acts) then
      print_endline ("Action is non-det: "^aname);
    non_det_acts := SetString.add aname !non_det_acts
      
  let to_copy { GProblem.conds = conds } =
    let to_copy_cond { GProblem.cond = cond } =
      Mylist.exists coincides cond
    in Mylist.exists to_copy_cond conds

  let setup { GProblem.actions = actions } =
    Mylist.iter (fun a -> if to_copy a then add_non_det_acts a) actions
      
  let copy_prefix = "copy----"
  let to_copy  { GProblem.aname = aname } =
    not (string_starts_with aname copy_prefix) &&
      SetString.mem aname !non_det_acts

  let num_copy = ref 1
  let add_copy = function { GProblem.actions = lactions; init = linit} as problem ->
    let prefix = (copy_prefix^(string_of_int !num_copy)^"-") in
    let atom2natom = ref(GProblem.SmallMapA.empty) in
    let make_new_lit l = 
      if not(coincides l) then
	(false,l)
      else
	let a = GProblem.Lit.atom_of_lit l in
	let nl na = 
	  if GProblem.Lit.lit_is_pos l then
	    GProblem.Lit.pos_atom na
	  else
	    GProblem.Lit.neg_atom na
	in try
	    let na = GProblem.SmallMapA.find a !atom2natom
	    in (true, nl na)
	  with Not_found ->
	    let na = GProblem.Lit.make_atom(prefix^(GProblem.Lit.A.to_text a))
	    in begin
		(* OJO: probablemente no necesario. ¿Tienen que estar en atoms_problem? Nope. Ej: KProblem *)
		MyArray.add_one !GProblem.atoms_problem na;

		atom2natom := GProblem.SmallMapA.add a na !atom2natom;
		(true, nl na)
	      end in
    let replace_lit l =
      let _,nl = make_new_lit l
      in nl in
    let proc_init new_init = function
	GProblem.Single l -> 
	  let added, nl = make_new_lit l
	  in if added then
	      (GProblem.Single nl)::new_init
	    else
	      new_init
      | GProblem.Clause lst -> 
	  let added_acc = ref(false)
	  and nlst = ref([])
	  in begin
	      List.iter
		(fun l ->
		   let added, nl = make_new_lit l
		   in if added then
		       begin
			 added_acc := true;
			 nlst := nl::!nlst;
		       end)
		lst;
	      if !added_acc then
		(GProblem.Clause !nlst)::new_init
	      else
		new_init 
	    end in
    let ninit = List.fold_left proc_init linit linit in
    let new_act { GProblem.aname = aname; conds = conds; prec = prec; eff = eff} =
      let replace_cond = 
	function { GProblem.cond = cond } as ce ->
       	  { ce with GProblem.cond = Mylist.map replace_lit cond } in
      let new_conds = Mylist.map replace_cond conds
      in (* print_endline ("New action "^prefix^aname); *)
	{ GProblem.aname = prefix^aname;	   
	   prec = Mylist.map replace_lit prec;
	   eff = Mylist.map replace_lit eff;
	   conds = new_conds; } in
    let proc_act acc act =
      if to_copy act then
	(new_act act)::acc
      else acc in
    let nacts = Mylist.fold proc_act [] lactions 
    in begin
	print_string "\nProcesando non-determinism (#";
	print_int !num_copy;
	print_endline ")";
	incr num_copy;

	(* adding actions to set of non-deterministic actions *)
	List.iter add_non_det_acts nacts;

	List.iter (fun a -> Mylist.add a lactions) nacts;
	{ problem with GProblem.init = ninit };
      end    

  type cache_entry = IsTrue | IsFalse | NoFill
  let cache = ref(NoFill)
  let rec is_nondeterministic = function { GProblem.init = linit} as prob ->
    match !cache with
	NoFill ->
	  let oneof =
	    try
	      collect_inits linit
	    with IsDeterministic -> []
	  in begin 
	      if oneof == [] then
		cache := IsFalse
	      else
		cache := IsTrue;
	      is_nondeterministic prob;
	    end
      | IsFalse -> false
      | IsTrue -> true
end



(*****************************************************)
(*****************    Relevance	  ********************)
(*****************************************************)

(*
  Doing Closure without events.
  
  Idea:
  - Worst case: Not necesary all relevant, but a long chain.
  In that case will be cuadratic on num of lits,
  when a linear can do it.

  - Best case: short path between atoms
  Seems to be faster...
*)
module Relevance =
struct
  let use_debug_relevance = ref(false)
  let debug_relevance = false

  (* relevant_to[l] = { lj | l -> lj } *)
  let relevant_to = GProblem.SparseMapL.empty ()

  (* to_relevant[l] = { lj | lj -> l } *)
  let to_relevant = GProblem.SparseMapL.empty ()

  let set_relevant_oneway tbl (l1:GProblem.Lit.t) (l2:GProblem.Lit.t) =
    let s =
      (try
	 GProblem.SparseMapL.find tbl l1
       with Not_found ->
	 let new_s = GProblem.SparseSetL.empty () in
	   begin
	     GProblem.SparseMapL.add l1 new_s tbl;
	     new_s;
	   end)
    in if not(GProblem.SparseSetL.mem s l2) then
	begin
          GProblem.SparseSetL.add l2 s;
	  true;
	end
      else
	false

  let made_change = ref(true) (* not safe, but run only once for each input Problem *) 
  (* l1 --> l2 *)
  let set_relevant (l1:GProblem.Lit.t) (l2:GProblem.Lit.t) =
    let print_rel s l1 l2 =
      begin
	GProblem.Lit.print_lit l1;
	print_string s;
	GProblem.Lit.print_lit l2;
	print_newline ();
      end in
    let b1 = set_relevant_oneway relevant_to l1 l2
    and b2 = set_relevant_oneway to_relevant l2 l1 in
      begin
	if debug_relevance & b1 then
	  print_rel " now relevant to " l1 l2;
	if debug_relevance & b2 then
	  print_rel " now relevant from " l2 l1;
	made_change := b1 or b2 or !made_change;
      end

  let is_relevant (l1:GProblem.Lit.t) (l2:GProblem.Lit.t) =
    try
      let s = GProblem.SparseMapL.find relevant_to l1 in
	GProblem.SparseSetL.mem s l2
    with Not_found ->
      false
	
  (* exist x \in l1* such that x relevant to l2 *)
  let is_relevant_star (l1:GProblem.Lit.t) (l2:GProblem.Lit.t) =
    let lit_star = GProblem.UnitPropL.hits_star [l1]
    in GProblem.SmallSetL.exists (fun lp -> is_relevant lp l2) lit_star
	
  let print_relevance_tbl () =
    let print_k_e sep k e =
      begin
	assert(GProblem.SparseSetL.mem e k);
	GProblem.Lit.print_lit k;
	print_string sep;
	GProblem.print_sparseset_lit e;
      end
    in begin
	print_string "=======================\na is Relevant to b: a -> b\n";
	GProblem.SparseMapL.iter (print_k_e "-> ") relevant_to;
	print_string "=======================\nInverse of relevance:\n";
	GProblem.SparseMapL.iter (print_k_e "<- ") to_relevant;
	print_newline()
      end
	 
  let self_relevant a =
    begin
      ignore(set_relevant (GProblem.Lit.pos_atom a) (GProblem.Lit.pos_atom a));
      ignore(set_relevant (GProblem.Lit.neg_atom a) (GProblem.Lit.neg_atom a));
    end

  let act_relevance  act =
    let ceff_relevance c effects =
      Mylist.iter
	(function e -> ignore(set_relevant c e))
	effects in
    let cond_relevance cond =
      Mylist.iter (function c ->
		   ceff_relevance c (GProblem.get_ceff cond))
	(GProblem.get_cond cond)
    in Mylist.iter cond_relevance (GProblem.get_conds act)

  let close_transitivity () =
    GProblem.SparseMapL.iter
      (fun lit lit_rel_to ->
	 try
	   let rel_to_lit = GProblem.SparseMapL.find relevant_to lit
	   in GProblem.SparseSetL.iter
		(fun li ->
		   GProblem.SparseSetL.iter
		     (fun lj ->
			set_relevant li lj
		     )
		     rel_to_lit (* { lj | lit -> lj } *)
		)
		lit_rel_to  (* { li | li -> lit } *)
	 with _ ->
	   begin
	     print_string "ignoring for relevance in transitivity:";
	     GProblem.Lit.print_lit lit;
	     print_newline();
	   end
      )
      to_relevant

  let close_negation () =
    GProblem.SparseMapL.iter
      (fun lit lit_rel_to ->
	 let nlit = GProblem.Lit.neg_lit lit
	 in GProblem.SparseSetL.iter
	      (fun li ->
		 set_relevant (GProblem.Lit.neg_lit li) nlit)
	      lit_rel_to  (* { li | li -> lit } *)
      )
      to_relevant

  let init_q () =
    begin
      GProblem.Lit.A.tbl_iter
	(fun _ s -> self_relevant s);
      Mylist.iter act_relevance (GProblem.get_actions());
    end
      
  let calc_relevance () =
    let count = ref(1) (* safe: reentrant *) in
    let rec fixpoint () =
      while !made_change do
	made_change := false;
	close_transitivity ();
	close_negation ();
	if debug_relevance then
	  begin
	    print_endline "====================";
	    print_string "After iteration ";
	    print_int !count;
	    print_endline ", relevance table:";
	    print_relevance_tbl ();
	  end;
	incr count;
      done
    in begin
	(* Resetting data structures *)
	GProblem.SparseMapL.clear to_relevant;
	GProblem.SparseMapL.clear relevant_to;
	
	init_q();
	(* OJO CONTINGENT: disjunction relevance not yet implemented. Necessary only for contingent *)
	fixpoint ();
	print_string "Relevance made ";
	print_int !count;
	print_endline " full iterations to fixpoint";
	if !use_debug_relevance then
	  print_relevance_tbl ();
      end

  (* { lj | lj -> ci } *)
  let find_to_relevant ci =
    GProblem.SparseMapL.find to_relevant ci

  (* { lj | ci -> lj } *)
  let find_relevant_to ci =
    GProblem.SparseMapL.find relevant_to ci
end
  

(*****************************************************)
(**************	  A tagged world	**************)
(*****************************************************)

module KAtom =
struct
  let tag_hash_val = 5381
  let f_hash vold vnew = 
    (vold lsl 5) + vold + vnew
  let rec l_hash vold = function
      [] -> vold
    | i::l -> l_hash (f_hash vold (GProblem.Lit.hash i)) l

  let list_lit_hash l =
    l_hash (tag_hash_val lsl 5) l
  let list_lit_to_text tag =
    let tag_text acc t =
      acc^"_"^(GProblem.Lit.to_text t)
    in List.fold_left tag_text "" tag

  let rec compare_list_lit l1 l2 = match (l1,l2) with
      [],[] -> 0
    | [],_ -> -1
    | _,[] -> 1
    | (a1::l1),(a2::l2) -> 
	let r = GProblem.Lit.compare a1 a2 
	in if r != 0 then r
	  else compare l1 l2

  module Tag =
  struct
    type t = GProblem.Lit.t list
    let compare x y = compare_list_lit x y
    let equal x y = (compare x y) == 0
    let hash = list_lit_hash
    let to_text = list_lit_to_text
    let nil = []
  end
  module T = Factory(Tag) 

  let tag_iter f tag =
    List.iter f (T.get tag)
  let tag_fold_left f acc tag =
    List.fold_left f acc (T.get tag)
  let tag_exists f tag =
    List.exists f (T.get tag)
  let tag_length tag =
    List.length (T.get tag)
  let tag_print tag =
    print_string (T.to_text tag)

  module SparseSetT = SparseSet(T)

  (* OJO: search for ocurrence of KAtom.T.get *)
  (* 	It would be better to use something else *)

  type tagged = Tagged of string * GProblem.Lit.t * T.t
  let k = "k"
  let make_tagged lit tag = Tagged(k,lit,tag)
    
  let compare_tagged l1 l2 = match (l1,l2) with
      Tagged(op1,_,_), Tagged(op2,_,_) when op1 != op2 -> compare op1 op2
    | Tagged(_,l1,tag1), Tagged(_,l2,tag2) ->
	let cl = GProblem.Lit.compare l1 l2
	in if cl != 0 then cl else T.compare tag1 tag2

  let text_tagged (Tagged(op,l,tag)) =
    if tag == T.nil then
      op^(GProblem.Lit.to_text l)
    else
      op^(GProblem.Lit.to_text l)^"_"^(T.to_text tag)
	
  let hash_tagged (Tagged(op,l,tag)) =
    let v1 = T.hash tag in
    let v2 = f_hash v1 (Hashtbl.hash(op))
    in f_hash v2 (GProblem.Lit.hash l)
	
  type t = tagged
  let compare = compare_tagged
  let equal x y = (compare_tagged x y) = 0
  let hash = hash_tagged
  let to_text = text_tagged
  let nil = Tagged("None",
		   (GProblem.Lit.neg_atom GProblem.Lit.A.nil),
		   T.nil)

  module FastMapT = FastMap(T)

  module type Func =
  sig
    type t
    val f: T.t -> t
  end
  module Cache(F:Func) =
  struct
    let cache = FastMapT.empty ()
    let reset () = FastMapT.clear cache
    let do_f lit = 
      try
	FastMapT.find cache lit
      with Not_found ->
	let res = F.f lit
	in begin
	    FastMapT.add lit res cache;
	    res;
	  end
  end

  module LT = struct
    type t = GProblem.Lit.t * T.t
    let compare (l1,t1) (l2,t2) =
      let c1 = GProblem.Lit.compare l1 l2 
      in if c1 != 0 then
	  c1
	else
	  T.compare t1 t2 
    let equal x y = (compare x y) == 0
    let l2 = log 2.0
    let hash (l,t) = 
      let s = int_of_float(log(float_of_int(T.num_elements()))/.l2)+1
      in ((GProblem.Lit.hash l) lsl s) + (T.hash t)
(*     let hash (l,t) = (GProblem.Lit.hash l) + (T.hash t) *)
    let num _ = raise (Failure "'num' not supported for module LT - see source code")
      (* we dont have a correlative num for index on array, etc *)
    let nil = (GProblem.Lit.nil,T.nil)
  end
    
  module SparseSetLT = SparseSet(LT)
  module SparseMapLT = SparseMap(LT)
  module type FuncL =
  sig
    type t
    val f: GProblem.Lit.t -> T.t -> t
  end
  module CacheL(F:FuncL) =
  struct
    let cache = SparseMapLT.empty ()
    let reset () = SparseMapLT.clear cache
    let do_f l t =
      let p = (l,t) 
      in try
	  SparseMapLT.find cache p
	with Not_found ->
	  let res = F.f l t
	  in begin
	      SparseMapLT.add p res cache;
	      res;
	    end
  end
end

module KProblem = Problem(KAtom)

let neg_tagged_a atom =
  match KProblem.Lit.A.get atom with
      (KAtom.Tagged(op,l,tag)) ->
	KProblem.Lit.A.make(KAtom.Tagged(op,GProblem.Lit.neg_lit l,tag))

let tag_of_tagged atom =
  match KProblem.Lit.A.get atom with
      (KAtom.Tagged(_,_,tag)) ->
	tag

let lit_of_tagged atom =
  match KProblem.Lit.A.get atom with
      (KAtom.Tagged(_,l,_)) ->
	l
let lit_and_tag_of_tagged atom =
  match KProblem.Lit.A.get atom with
      (KAtom.Tagged(_,l,tag)) ->
	(l,tag)

let double_neg l =
  KProblem.Lit.neg_lit
    (KProblem.Lit.trans_atom_in_lit neg_tagged_a l)

let is_tagged l t = 
  KProblem.Lit.A.tbl_mem (KAtom.make_tagged l t)

let neg_tagged_l l =
  KProblem.Lit.trans_atom_in_lit neg_tagged_a l
let lit_of_tagged_l l =
  lit_of_tagged (KProblem.Lit.atom_of_lit l)

(* Verification desactivated by using FALSE *)
let check_not_creating_klt = false
(* Maybe it should be inside the Factory, but
1. Do we want it for all instances of Factory? Nop.
2. We don't want any overhead inside Factory,
the best ocaml code of this planner B-) *)

let allow_create_klt = ref(true)  (* safe: option set once *)
let disable_creation_klt () =
  allow_create_klt := false

module Tag2lit = struct
  let cache = ref(GProblem.FastMapL.empty ())
  let cache_set = ref(false)
  let for_klt _ klt =
    let (_,tag) = lit_and_tag_of_tagged klt in
    let for_tag t = 
      let set = 
	try
	  GProblem.FastMapL.find !cache t
	with Not_found ->
	  let n = KProblem.SparseSetA.empty()
	  in GProblem.FastMapL.add t n !cache;
	    n
      in KProblem.SparseSetA.add klt set;
    in KAtom.tag_iter for_tag tag

  let print () =
    begin
      print_endline "----------------------------------------";
      print_endline "Tag2Lit";
      GProblem.FastMapL.iter
	(fun l klt_set -> 
	   print_string "For lit: ";
	   GProblem.Lit.print_lit l;
	   print_endline " the following atoms:";
	   KProblem.SparseSetA.iter KProblem.Lit.print_atom klt_set;
	   print_newline();
	)
	!cache;
      print_endline "----------------------------------------";
    end      

  let setup () =
    begin
      assert( not !allow_create_klt );
      cache_set := true;
      cache := GProblem.FastMapL.empty ();
      KProblem.Lit.A.tbl_iter for_klt;
      if false then
	print ();
    end

  let f t =
    assert( !cache_set );
    GProblem.FastMapL.find !cache t
end

(*
   OJO CONTINGENT: generalize for different kind of tags: K, K0, M, etc...
*)
let klt lit tag = 
  assert( not check_not_creating_klt or !allow_create_klt or is_tagged lit tag);
  KProblem.Lit.make_atom (KAtom.make_tagged lit tag)
let knlt lit tag = 
  assert( not check_not_creating_klt or !allow_create_klt or is_tagged (GProblem.Lit.neg_lit lit) tag);
  KProblem.Lit.make_atom (KAtom.make_tagged (GProblem.Lit.neg_lit lit) tag)

let pklt l tag = KProblem.Lit.pos_atom (klt l tag)
let pknlt l tag = KProblem.Lit.pos_atom (knlt l tag)
let nknlt l tag = KProblem.Lit.neg_atom (knlt l tag)
let nklt l tag = KProblem.Lit.neg_atom (klt l tag)

let kl lit = klt lit KAtom.T.nil
let knl lit = knlt lit KAtom.T.nil

let pkl l = pklt l KAtom.T.nil
let pknl l = pknlt l KAtom.T.nil
let nknl l = nknlt l KAtom.T.nil
let nkl l = nklt l KAtom.T.nil

(******** Last ocurrence of Tagged   *********)


(*****************************************************)
(************** Known and Unknown Atoms **************)
(*****************************************************)

module Uncertainty =
struct
  let use_debug_c_i_l = ref(false)
  (* Atoms not mentioned in init, are assumed to be unknown *)
  (* In case that what is passed from the C++ code change,
     (for example, for providing trivial clauses, and closed world assumption )
     just change here.
     After this, Problem.assert_init should pass
  *)
  let fix_init = function { GProblem.init = linit } as myproblem ->
    let atoms = GProblem.get_atoms_set myproblem in
    let mentioned_lits = GProblem.set_from_state_and_clauses linit in
    let mentioned_atoms = GProblem.set_lit_to_set_atom mentioned_lits in
    let non_mentioned_atoms = GProblem.FastSetA.diff atoms mentioned_atoms in
    let new_clauses = GProblem.atom_to_trivial_clauses_set non_mentioned_atoms
    in GProblem.set_init myproblem (new_clauses@linit)

(*   let fix_goal = function { GProblem.init = linit; goal = lgoal } as myproblem -> *)
(*     let ninit = ref(linit) in *)
(*     let ngoal = ref(lgoal) in *)
(*     let add_lit r lit = r := (Single lit)::!r in *)
(*     let allow_end = GProblem.Lit.make_atom("ALLOW_END") *)
(*     in begin *)
(* 	add_lit ninit (GProblem.Lit.pos_atom allow_end); *)	
(*       end *)

  let known_atoms = ref(GProblem.SmallSetA.empty) (* safe for reentrant code. Is reset in setup_known_unknown  *)
  let unknown_atoms = ref([]:GProblem.Lit.A.t list) (* safe for reentrant code. Is reset in setup_known_unknown *)

  let calc_known_atoms () =
    let proc_item acc = function
	GProblem.Clause _ -> acc
      | GProblem.Single lit ->
	  let l = GProblem.Lit.atom_of_lit lit
	  in GProblem.SmallSetA.add l acc
    in List.fold_left proc_item GProblem.SmallSetA.empty (GProblem.get_init())

  let calc_unknown_atoms () =
    let proc_atom a acc =
      if GProblem.SmallSetA.mem a !known_atoms then
	acc
      else
	a::acc
    in GProblem.FastSetA.fold proc_atom (GProblem.get_atoms_set !GProblem.problem) []

  let print_known_and_unknown() =
    begin
      print_string "\nAtoms with known state: ";
      GProblem.SmallSetA.iter GProblem.Lit.print_atom !known_atoms;
      print_string "\nAtoms with Unknown state: ";
      List.iter GProblem.Lit.print_atom !unknown_atoms;
      print_newline();
    end

  let setup_known_unknown() =
    begin
      known_atoms := calc_known_atoms ();
      unknown_atoms := calc_unknown_atoms();
      if !Verbose.level > Verbose.level1 then
	print_known_and_unknown();
    end


  (*****************************************************)
  (**************    Calculate Merges     **************)
  (*****************************************************)


  (*
    Is not necessary to use a fastest data structure.
    This is used through a cached function, anyway
  *)
  let clauses_list = ref([]: GProblem.Lit.t list list) (* safe for reentrant code as reset by setup_clauses *)
    
  let setup_clauses use_k0 =
    let clauses_init = List.fold_left GProblem.get_clause_list [] (GProblem.get_init ()) in
    let atoms_mentioned = 
      List.fold_left 
	(fun acc c ->
	   List.fold_left
	     (fun acc lit ->
		GProblem.SmallSetA.add (GProblem.Lit.atom_of_lit lit) acc)
	     acc
	     c)
	GProblem.SmallSetA.empty
	clauses_init in
    let final_clauses =
      GProblem.SmallSetA.fold
	(fun a acc -> 
	   [GProblem.Lit.pos_atom a; GProblem.Lit.neg_atom a ]::acc)
	atoms_mentioned
	clauses_init
    in begin
	if false then
	  clauses_list := final_clauses
	else (* enforce prime implicate form *)
	  begin 
	    if (not use_k0) && false then
	      clauses_list := 
	      Timed.call "Getting clauses in PI form"
		(fun () ->
		   GProblem.get_in_pi_form final_clauses)
	    else
	      clauses_list := final_clauses;
	  end
      end


  let clause_relevant_to_l l clause =
    List.for_all (fun lit -> Relevance.is_relevant lit l) clause

  let debug_hitting = false

  module Fun_c_i =
  struct
    type t = GProblem.Lit.t list list
    let f lit =
      let res = List.filter (clause_relevant_to_l lit) !clauses_list
      in begin
	  if debug_hitting then
	    begin
	      print_string "C_I(";
	      GProblem.Lit.print_lit lit;
	      print_endline ") calculated is ";
	      List.iter GProblem.print_list_lit res;
	      print_endline "------------------------------";
	    end;
	  res;
	end
  end
  module C_i = GProblem.Lit.Cache(Fun_c_i) (* Doesn't need to be reset for reentrance: GProblem used once *)
  let c_i lit = C_i.do_f lit

  (* hitting[l] = { c | c hits c_i(l) } *)
  let hitting = GProblem.FastMapL.empty () (* should be safe for reentrant code *)

  (* for lits not precond or goal *)
  let other_hitting = GProblem.FastMapL.empty () (* should be safe for reentrant code *)

  (*
    For all lit \in c,
    --- lit hits* ci
  *)
  let clause_hits_ci c ci =
    let lit_hits_ci lit =
      let lit_star = GProblem.UnitPropL.hits_star [lit]
      in List.exists (fun lit2 -> GProblem.SmallSetL.mem lit2 lit_star) ci in
    let res = List.for_all lit_hits_ci c
    in if debug_hitting then
	begin
	  GProblem.print_list_lit c;
	  if res then
	    print_string " HITS "
	  else
	    print_string " DOESN'T HIT ";
	  GProblem.print_list_lit ci;
	  print_endline "because:";
	  List.iter 
	    (fun lit ->
	       let lit_star = GProblem.UnitPropL.hits_star [lit]
	       in begin
		   GProblem.Lit.print_lit lit;
		   print_string " star is: ";
		   GProblem.SmallSetL.iter GProblem.Lit.print_lit lit_star;
		   print_newline();
		   try
		     List.iter
		       (fun lit2 -> 
			  if GProblem.SmallSetL.mem 
			    lit2 lit_star then
			      begin
				print_string " and ";
				GProblem.Lit.print_lit lit2;
				print_endline " is in lit_star";
				raise (Failure "")
			      end)
		       ci;
		     print_endline "but no one is in lit_star";
		   with Failure _ -> ()
		 end
		 )
	    c;
	  print_endline "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%";
	end;
      res

  let print_status_c_i () =
    let status_l l =
      let c_i_l = c_i l
      in begin
	  print_string "C_I(";
	  GProblem.Lit.print_lit l;
	  print_string ") = ";
	  List.iter GProblem.print_list_lit c_i_l;
	  print_newline();
	end
    in
      begin
	assert( List.length !GProblem.goals_and_precs > 0 );
	List.iter status_l !GProblem.goals_and_precs
      end
	
  let diagnose_hitting_clause use_k0 =
    let check_l acc l =
      try 
	if use_k0 then 
	  acc
	else
	  if (c_i l != [] &&
	      GProblem.FastMapL.find hitting l == [] ) then
	    begin
	      print_string "Warning: literal";
	      GProblem.Lit.print_lit l;
	      print_string " is not accepted for just disjunction. Width > 1";
	      print_newline();
	      false
	    end
	  else
	    acc
      with Not_found ->
	begin
	  print_string "Not found in diagnose hitting for lit:";
	  GProblem.Lit.print_lit l;
	  print_newline();
	  acc
	end
    in begin
	assert( List.length !GProblem.goals_and_precs > 0 );
	if not (List.fold_left check_l true !GProblem.goals_and_precs) then
	  begin
	    print_endline ("Warning: some preconditions or literals not " ^
			     "accepted for just one disjunction. Width > 1");
	    print_endline "Problem can be unsolvable";
	  end
      end

  let get_the_merge_s0 c_i_l =
    let models_lit = GProblem.get_models_lits c_i_l
    in List.map (fun ti -> KAtom.T.make ti) models_lit

  (* 
     Calculate hitting set for c_i_l
  *)

  let debug_hit_models = false
    (* OJO: Veeeery slow with blocks p02 in test-cf2cs. Probably string concat.
       Big cnf: 42k lines.
       So, slow c2d_220.
       even worst, buggy, returning, maybe, an empty-merge.
    *)
  let save_cnf_hit cnf_nf clauses =
    let str_clauses = ref("") (* safe: reentrant *) in
    let n_clauses = ref(0) (* safe: reentrant *) in
    let add_to_clause lit =
      str_clauses := !str_clauses ^ ((string_of_int lit)^" ") in
    let finish_clause () =
      incr n_clauses;
      str_clauses := !str_clauses ^ "0\n" in

    let get_equiv l l2 =
      begin
	if debug_hit_models then
	  begin
	    print_string "equivalence between: ";
	    print_int l;
	    print_string " and ";
	    print_int l2;
	    print_newline();
	  end;
	add_to_clause l;
	add_to_clause (-l2);
	finish_clause();
	add_to_clause (-l);
	add_to_clause l2;
	finish_clause();
      end in
    let get_contradiction l l2 =
      begin
	if debug_hit_models then
	  begin
	    print_string "contradiction between: ";
	    print_int l;
	    print_string " and ";
	    print_int l2;
	    print_newline();
	  end;
	add_to_clause l;
	add_to_clause l2;
	finish_clause();
	add_to_clause (-l);
	add_to_clause (-l2);
	finish_clause();
      end in
    let get_sign l =
      l > 0 in
    let save_equiv atom_to_var =
      let rec proc_item l = function
	  [] -> ()
	| l2::lst -> 
	    begin
	      if (get_sign l) == (get_sign l2) then
		get_equiv (abs l) (abs l2)
	      else
		get_contradiction (abs l) (abs l2);
	      proc_item l lst;
	    end in
      let rec proc_list = function
	  [] -> ()
	| l::lst -> proc_item l lst;
	    proc_list lst
      in GProblem.SmallMapA.iter (fun _ lst -> 
(* 				    str_clauses :=  *)
(* 				      !str_clauses ^ "c " ^ *)
(* 					(GProblem.Lit.A.to_text a) ^ "\n"; *)
				    proc_list lst) atom_to_var in

    let make_lit ci atom_to_var next_var =
      let atom = GProblem.Lit.atom_of_lit ci in
      let sign = if GProblem.Lit.lit_is_pos ci then 1 else -1 in 
      let old_l = 
	try GProblem.SmallMapA.find atom atom_to_var
	with Not_found -> [] in
      let natom_to_var = GProblem.SmallMapA.add atom (sign*next_var::old_l) atom_to_var
      in natom_to_var, (next_var+1) in
    let rec clause_to_int next_var atom_to_var = function
	[] -> finish_clause (); next_var, atom_to_var
      | ci::c -> 
	  let natom_to_var,nnext_var = make_lit ci atom_to_var next_var
	  in add_to_clause (nnext_var-1);
	    clause_to_int nnext_var natom_to_var c in
    let rec aux next_var atom_to_var = function
	[] -> next_var, atom_to_var
      | c::lst ->
	  let nnext_var, natom_to_var =
	    clause_to_int next_var atom_to_var c 
	  in aux nnext_var natom_to_var lst in
    let next_var, atom_to_var = aux 1 GProblem.SmallMapA.empty clauses in
    let cnf = open_out cnf_nf
    in begin
	if debug_hit_models then
	  begin
	    print_endline "Original clauses are:";
	    List.iter GProblem.print_list_lit clauses;
	    print_endline "Atom to var:";
	    GProblem.SmallMapA.iter
	      (fun atom var -> GProblem.Lit.print_atom atom;
		 print_string ": "; List.iter (fun l -> print_int l; print_string " ") var; print_newline())
	      atom_to_var
	  end;
	save_equiv atom_to_var;
	output_string cnf ("p cnf "^(string_of_int (next_var-1))^" "^(string_of_int !n_clauses)^"\n");
	output_string cnf !str_clauses;
	close_out cnf;
        ((next_var-1), atom_to_var);
      end

  exception Max_models
  let get_models_hit cnf_nf =
    let max_num_models = 10000 in (* Maximum number of hitting sets accepted. Otherwise it fails *)
    let out_c2d = Unix.open_process_in ("$TRANSLATOR_HOME/c2d_220 -in "^cnf_nf^
					  " -smooth_all -dt_method 4 -count > output-c2d.log     ;"
					^"$TRANSLATOR_HOME/models --write-min-models "^
					  "--num "^(string_of_int max_num_models)^" "^
					  cnf_nf^".nnf |tee output-models.log") in
    let models = ref([]) (* safe: reentrant *) in
    let get_str_lst line =
      (Str.split 
	 (Str.regexp " ") 
	 (List.hd (Str.split (Str.regexp "{") 
		     (List.hd (Str.split (Str.regexp "}") line))))) in
    let get_a_model line =
      if debug_hit_models then
	begin
	  print_string "line: ";
	  List.iter
	    (fun x -> print_string ("'"^x^"' "))
	    (get_str_lst line);
	  print_newline();
	end;
      let model = List.map int_of_string (get_str_lst line)
      in begin
	  if debug_hit_models then
	    begin
	      print_string "Model: ";
	      List.iter (fun i -> print_int i; print_string " ")
		model;
	      print_newline ();
	    end;
	  model;
	end in
    let get_num_models line =
      try
	let s = Str.split (Str.regexp " ") line in
	let field = Str.split (Str.regexp "=") (List.nth s 3)
	in int_of_string(List.nth field 1)
      with _ -> print_endline "ERROR in get_num_models"; -1 (* Failure *)
    in try
	let go = ref(true) in  (* safe: reentrant *)
	let seeing_model = ref(false) in (* safe: reentrant *)
	let num_models = ref(-1) in (* safe: reentrant *)
	let num_min_models = ref(0) 
	in while !go do
	    let line = input_line out_c2d
	    in
	      if string_starts_with line "--- models begin" then
		seeing_model := true
	      else if string_starts_with line "---- models end" then
		go := false
	      else if string_starts_with line "main: #" then
		num_models := get_num_models line
	      else if !seeing_model then
		begin
		  models := (get_a_model line)::!models;
		  incr num_min_models;
		  if !num_min_models == max_num_models then
		    begin
		      print_endline ("ERROR: maximum number of hitting sets:"^(string_of_int max_num_models));
		      print_endline "You can increase it in source code, but it will probably create huge unsolvable PDDLs";
		      raise Max_models;
		    end
		end
	      (* else ignore line *)
	  done;
	  ignore(Unix.close_process_in out_c2d);
	  !models
      with a ->
	print_endline ("Error while calling 'c2d' or 'models' over file "^cnf_nf);
	print_endline "Be sure 'c2d' and 'models' are in $TRANSLATOR_HOME";
	raise a

  let get_merge_hit atom_to_var models =
    let var_to_lit =
      GProblem.SmallMapA.fold
	(fun atom lst acc ->
	   List.fold_left 
	     (fun acc var ->
		if var > 0 then
		  MapInt.add var (GProblem.Lit.pos_atom atom) acc
		else
		  MapInt.add (abs var) (GProblem.Lit.neg_atom atom) acc)
	     acc
	     lst)
	atom_to_var
	MapInt.empty
    in List.map
	 (fun model ->
	    List.map
	      (fun nlit ->
		 try
		   MapInt.find nlit var_to_lit
		 with Not_found ->
		   print_string "Not found var: ";
		   print_int nlit;
		   print_newline();
		   raise (Failure "error in get_merge"))
	      model)
	 models

  let get_the_merge_hit c_i_l =
    let cnf_nf = ".c_i_l.cnf" in
    let _, atom_to_var = save_cnf_hit cnf_nf c_i_l in (* OJO OJO, retorno extra. quitar *)
    let models = 
      Timed.call "Calculating hitting set for merge"
	(fun () ->
	   get_models_hit cnf_nf) in
    let models_lit = get_merge_hit atom_to_var models
    in List.map (fun ti -> KAtom.T.make ti) models_lit
	 
  (* 
     Calculate once for each c_i_l
     if all relevant to all, will be calculated once.
  *)

  module MyMap = Map.Make(struct
			    type t = GProblem.Lit.t list list
			    let compare = compare
			  end)
  let s_my_cache () = MyMap.empty
  let my_cache = ref(s_my_cache ()) (* safe reentrant as reset by setup_hitting *)
  let get_merge_for_cil l c_i_l =
    let get_merge c_i_l = 
      try
	MyMap.find c_i_l !my_cache
      with Not_found ->
	(let nc_i_l () =
	   let vars = (* vars used in c_i_l *)
	     List.fold_left 
	       (fun acc cls ->
		  List.fold_left 
		    (fun acc l ->
		       GProblem.SmallSetA.add (GProblem.Lit.atom_of_lit l) acc)
		    acc
		    cls)
	       GProblem.SmallSetA.empty !clauses_list in
	   let filtered_cls = 
	     List.filter (* global clauses with vars in c_i_l *)
	       (List.for_all (fun l -> GProblem.SmallSetA.mem (GProblem.Lit.atom_of_lit l) vars))
	       !clauses_list
	   in (* Filtered cls = Project(I, vars(C_I(L)), so models of C_I(L) are consistent with I *)
	     List.append c_i_l filtered_cls in
	 let c_i_l = nc_i_l() in
	 let the_merge = 
	   try get_the_merge_s0 c_i_l
	     (* OJOOO OJO: try to replace by get_the_merge_hit 
		try get_the_merge_hit c_i_l
		Gave a bug for IPC6.
	     *)
	   with Max_models -> 
	     begin
	       print_endline "Not getting a merge";
	       raise Max_models
	     end
	     | _ -> get_the_merge_s0 c_i_l
	 in my_cache := MyMap.add c_i_l the_merge !my_cache;
	   the_merge)
    in GProblem.FastMapL.add l [get_merge c_i_l] hitting
      
  let get_dnf_of_clauses m =
    let get_dnf_of_a_clause c =
      List.map (fun x -> KAtom.T.make [x]) c
    in List.map get_dnf_of_a_clause m
  let from_dnf_to_clause m =
    List.map (fun t ->
		match KAtom.T.get t with
		    [x] -> x 
		  | _ -> raise (Failure "element is not a singleton [_]")) 
      m

  let do_merge_all = ref(false)
  let cache_mergeable = ref([])
  (*
    for each L prec or goal
    --- calc C_i(L) for every lit
    ------- calc each c \in C_i(L) that hit (or hits* ) C_i(L)
  *)
  let setup_hitting use_also_real_k1 use_real_k1 use_old_t0 use_s0 use_force_s0 use_k0 option_do_merge_all =
    let add_all_merge_1 l lst = 
      GProblem.FastMapL.add l
	(List.fold_left (fun acc m ->
			   List.append (get_dnf_of_clauses [m]) acc)
	   [] lst) hitting in
    let add_merge_width_1 l hits_all =
      begin
	print_string "width 1 for literal:";
	GProblem.Lit.print_lit l;
	print_newline ();
	match hits_all with
	    [] -> raise (Failure "in setup_hitting")
	  | [_] as m-> GProblem.FastMapL.add l (get_dnf_of_clauses m) hitting
	  | (_::m::_) as lst -> begin 
	      (* 2nd merge. Thumb rule to get positive clauses of (oneof a b). Sound anyway *)
	      print_string "For goal";
	      GProblem.Lit.print_lit l;
	      print_string " there is more than one merge. ";
	      if true then (* using only one merge. Complete *)
		begin
		  print_endline "Picking the first";
		  GProblem.FastMapL.add l (get_dnf_of_clauses [m]) hitting;
		end
	      else
		begin (* Can be faster? *)
		  print_endline "Adding all";
		  add_all_merge_1 l lst;
		end;
	    end;
	      
	if debug_hitting then
	  begin
	    print_string "Accepted clauses:\n";
	    GProblem.Lit.print_lit l;
	    print_string " accepts ";
	    List.iter
	      GProblem.print_list_lit
	      hits_all;
	  end;
      end in
    let setup_hitting_l l =
      if use_k0 then (* empty merges *)
	GProblem.FastMapL.add l [] hitting
      else
	(let c_i_l = c_i l 
	in if c_i_l != [] then
	  if use_real_k1 then
	    add_all_merge_1 l c_i_l
	  else if use_force_s0 then
	    begin
	      print_string "Width > 0 for literal:";
	      GProblem.Lit.print_lit l;
	      print_endline ". Trying with tags = Models{ Init }. Exponential but complete. (Forcing use of tags {s0})";
	      get_merge_for_cil l !clauses_list;
	    end
	  else
	    let do_hits_all c =
	      List.for_all (clause_hits_ci c) c_i_l in
	    let hits_all = (List.filter do_hits_all c_i_l)
	    in if hits_all != [] then
		add_merge_width_1 l hits_all
	      else if use_s0 then
		begin
		  print_string "Width > 1 for literal:";
		  GProblem.Lit.print_lit l;
		  print_endline ". Trying with tags = Models{ Clauses relevant to l }. Exponential but complete";
		  get_merge_for_cil l c_i_l;
		end
	      else if use_also_real_k1 then
		add_all_merge_1 l c_i_l
	      else if use_old_t0 then (* empty merges *)
		GProblem.FastMapL.add l [] hitting
	      else
		begin
		  print_endline "FATAL ERROR. Not a clear strategy for generating merges.";
		  print_endline "call cf2cs without arguments to see options: -ak1, -s0, etc.";
		  exit(100);
		end)
    in
      begin
	C_i.reset();
	GProblem.FastMapL.clear hitting;
	GProblem.FastMapL.clear other_hitting;
	my_cache := s_my_cache ();
	setup_clauses use_k0;
	GProblem.setup_goals_and_precs();
	do_merge_all := option_do_merge_all;
	cache_mergeable := [];

	assert( List.length !GProblem.goals_and_precs > 0 );
	List.iter setup_hitting_l !GProblem.goals_and_precs;

	diagnose_hitting_clause use_k0;
	if !use_debug_c_i_l || debug_hitting then
	  print_status_c_i();
      end

  let merges g =
    if (c_i g) == [] then
      []
    else
      try
	GProblem.FastMapL.find hitting g
      with Not_found ->
	begin
	  (* print_endline "Error: there is not hitting clause for"; *)
	  (* GProblem.Lit.print_lit g; *)
	  raise (Failure "there is not hitting clause for");
	end

  let find_same_c_i_l c_i_l = 
    let is_same_c_i_l = fun ((found,_) as acc) g ->
      if found then
	acc
      else
	if (c_i g) = c_i_l then (* OJO: expensive comparison, and maybe not safe *)
	  true,merges g
	else
	  acc
    in List.fold_left is_same_c_i_l (false,[]) !GProblem.goals_and_precs

  let get_all_merges () = 
    let do_acc acc g =
      (merges g)@acc
    in List.fold_left do_acc [] !GProblem.goals_and_precs
    
  let calc_extended_merges l =
    let c_i_l = c_i l 
    in if c_i_l == [] then (* or use_k0 then *)
	[]
      else
	let found,m = find_same_c_i_l c_i_l
	in if found then m
	  else
	    let all_merges = get_all_merges () in
	    let all_possible_merges = 
	      List.filter 
		(fun m -> 
		   try
		     let c = from_dnf_to_clause m
		     in List.exists (fun x -> x == c) c_i_l 
		   with Failure _ ->  false)
  		all_merges
	    in all_possible_merges
		 
  (* Get merges for l, being it a precond, goal or not *)
  let extended_merges l =
    try
      merges l
    with Failure _ ->
      try
	GProblem.FastMapL.find other_hitting l
      with Not_found ->
	(* Try to merge l by other ways *)
	let res = calc_extended_merges l
	in begin
	    if false then
	      begin
		print_string "For literal ";
		GProblem.Lit.print_lit l;
		print_endline " got the following merges";
		List.iter 
		  (fun mi -> 
		     List.iter KAtom.tag_print mi;
		     print_newline())
		  res;
		print_endline "----------------------------------------";
	      end;
	    GProblem.FastMapL.add l res hitting;
	    res
	  end

  (* return atoms to be merged *)
  let mergeable () =
    if !cache_mergeable == [] then
      begin
	if !do_merge_all then
	  cache_mergeable := GProblem.get_all_lits !GProblem.problem
	else
	  cache_mergeable := !GProblem.goals_and_precs
      end;
    !cache_mergeable
end

(*****************************************************)
(**************	    First K Problem	**************)
(*****************************************************)

(* OJO: for CONTINGET, look for a general mechanism for ramification *)
module RamificationMutex =
struct
  let atoms_map_k = ref(KProblem.SmallMapL.empty) (* safe as is reset by setup *)

  let setup () =
    atoms_map_k := KProblem.SmallMapL.empty

  let calc_expand_k klit =
    try
      KProblem.SmallMapL.find klit !atoms_map_k
    with Not_found ->
      let res = Mylist.empty() in
      let (l,tag) = lit_and_tag_of_tagged (KProblem.Lit.atom_of_lit klit)
      in begin
	  begin
	    if KProblem.Lit.lit_is_pos klit && tag == KAtom.T.nil then
	      begin
		let mutex_with_l = GProblem.mutex_with_l l in
		let each_lp lp =
		  if false then
		    begin
		      print_string "Ramificando: ";
		      KProblem.Lit.print_lit klit;
		      print_string " -> ";
		      KProblem.Lit.print_lit (pknl lp);
		      print_newline();
		    end;
		  Mylist.add (pknl lp) res
 		in GProblem.MutexC.iter each_lp mutex_with_l
	      end;
	  end;
	  atoms_map_k := KProblem.SmallMapL.add klit res !atoms_map_k;
	  res
	end

  let expand_k klit =
    let res = Mylist.empty ()
    in begin
	Mylist.add klit res;
        Mylist.append_to res (calc_expand_k klit);
	res
      end
end

module Translate =
struct
  let use_invariants = false (* In conformant is not necessary, but in contingent? *)
  
  (* OJO: magic references to this string. Unstable *)
  let merge_prefix = "MERGE"
    
  let have_cost aname = 
    not (string_starts_with aname merge_prefix)

  (*
    For each action in P,
    - trans prec and eff

    - for each cond-effect: C -> L, also TRUE -> L
    --- for each C -> li
    ----- if (1) li relevant to g 
    -----  & (2) m is a merge of g
    -----  & (3) ti \in merge m
    --------- add KC/ti -> Kli/ti
    --------- replace Ka/ti by Ka if ti not relevant to a 
    
    ----- if (1) -li relevant to g 
    -----  & (2) m is a merge of g
    -----  & (3) ti \in merge m
    --------- add -K-C/ti -> -K-li/ti
    --------- replace Ka/ti by Ka if ti not relevant to a 
    
    Also add merges
  *)
  (**************          K1             **************)
  (*****************************************************)

  module Fun_get_relevant_goals =
  struct
    type t = GProblem.Lit.t list

    (* Atoms relevant to a goal.
       This is not enough refined,
       but works for getting candidates.
       Concrete relevance are check in "new_conds"
    *)
    let f atom =
      let lit = GProblem.Lit.pos_atom atom
      and nlit = GProblem.Lit.neg_atom atom
      in List.filter
	   (fun g ->
	      (Relevance.is_relevant lit g)
	      || (Relevance.is_relevant nlit g))
	   !GProblem.goals_and_precs
  end
    (* Doesn't need to be reset for reentrance: GProblem used once *)
  module Get_relevant_goals = GProblem.Lit.CacheA(Fun_get_relevant_goals)
  let get_relevant_goals atom = Get_relevant_goals.do_f atom

  let debug_k1 = false

  (* OJO: magix references to these strings. Unstable. *)
  let cancel_comment = " Cancellation"
  let support_comment = " Support"

  module Fun_exists_in_tag_star =
  struct
    type t = bool
    let f l t =
      KAtom.tag_exists (fun ti -> Relevance.is_relevant_star ti l) t
  end
  module Exists_in_tag_star = KAtom.CacheL(Fun_exists_in_tag_star)

  module Func_tag_hits_star =
  struct
    type t = GProblem.SmallSetL.t
    let f t = GProblem.UnitPropL.hits_star (KAtom.T.get t) 
  end
  module Tag_hits_star = KAtom.Cache(Func_tag_hits_star)

  (* Get new conds effects from one cond effect *)
  let new_conds acc_conds = function { GProblem.cond = cond; ceff = ceff } as condeff ->
    let processed_ti = KAtom.SparseSetLT.empty () in

    let ok_cond_kl_t l ti = 
      Exists_in_tag_star.do_f l ti in

    let ok_eff_kl_t l ti = 
      KAtom.tag_length ti = 0 or Exists_in_tag_star.do_f l ti in
	
    (* precond: e or -e is relevant to g, with merge m, and ti \in m *)
    (* we don't know whether some tii is relevant to e, with tii \in ti *)
    let new_cond_e_ti e ti =
      let support_conds =
	Mylist.map (fun ci ->
		      if ok_cond_kl_t ci ti then
			pklt ci ti
		      else
			pkl ci
		   ) 
	  cond in
      let cancel_conds =
	Mylist.map (fun ci ->
		      let nci = (GProblem.Lit.neg_lit ci) in
		      if ok_cond_kl_t nci ti then
			nklt nci ti
		      else
			nkl nci
		   ) 
	  cond 
      in begin
	  (* Support *)
	  (if ok_eff_kl_t e ti then
	     let add_effect = Mylist.empty ()
	     in begin
		 Mylist.add (pklt e ti) add_effect;
		 if use_invariants then
		   Mylist.add (nknlt e ti) add_effect;
		 Mylist.add 
		   { KProblem.ccomment = support_comment;
		     cond = support_conds;
		     ceff = add_effect }
		   acc_conds;
	       end);
	  (* Cancellation *)
	  (let ne = GProblem.Lit.neg_lit e
	   in if ok_eff_kl_t ne ti then
	       let cancel_effect = Mylist.empty () 
	       in begin
		   Mylist.add (nklt ne ti) cancel_effect;
		   Mylist.add
		     { KProblem.ccomment = cancel_comment;
		       cond = cancel_conds;
		       ceff = cancel_effect }
		     acc_conds;
		 end);
	end in

    (* precond: e or -e is relevant to g, with merge m, and ti \in m *)
    let new_cond_e_ti_t e ti =
      if not (KAtom.SparseSetLT.mem processed_ti (e,ti)) then
	begin
	  KAtom.SparseSetLT.add (e,ti) processed_ti;
	  new_cond_e_ti e ti
	end in

    (* precond: e or -e is relevant to g with a merge m *)
    let new_cond_e_merge e m = 
      List.iter (new_cond_e_ti_t e) m in

    (* precond: e or -e is relevant to g *)
    let new_conds_e_g e g =
      begin
	if debug_k1 then
	  begin
	    print_string "********* ";
	    print_endline " generating for condition: ";
	    GProblem.print_cond_eff condeff;
	    print_string " for effect";
	    GProblem.Lit.print_lit e;
	    print_string " and goal";
	    GProblem.Lit.print_lit g;
	    print_newline ();
	  end;
        List.iter (new_cond_e_merge e) (Uncertainty.merges g)
      end in
    let new_conds_e e =
      if debug_k1 then
	begin
	  print_string "       effect:";
	  GProblem.Lit.print_lit e;
	  print_newline ();
	end;
      new_cond_e_ti e KAtom.T.nil; (* empty tag *)
      List.iter (new_conds_e_g e) (get_relevant_goals (GProblem.Lit.atom_of_lit e))
    in
      if debug_k1 then
	begin
	  print_string "Seeing effects:";
	  Mylist.iter GProblem.Lit.print_lit ceff;
	  print_newline ();
	end;
      Mylist.iter new_conds_e ceff;
      acc_conds

  (*
    For UNCONDITIONAL effects
    Behave as if have rule TRUE -> eff
    So, make a dummy effect, generated new effects,
    and collect them all.

    question: if I'm going to delete cancellation
    rules, should I distinguish this rules
    and compact them later?

    Maybe a better implementation is to enconde uncond effects
    as cond-effects with empty conds
  *)
  let debug_get_effects = false
  let (get_effects: GProblem.Lit.t Mylist.t -> KProblem.Lit.t Mylist.t) = fun eff ->
    let temp_cond = { GProblem.ccomment = ""; cond = Mylist.empty(); ceff = eff }
    in if debug_get_effects then
	begin
	  print_string "temporal cond: ";
	  GProblem.print_cond_eff temp_cond;
	end;
      let temp_condeffs = new_conds (Mylist.empty ()) temp_cond in
      let collect_effects acc ce =
	begin
	  if debug_get_effects then
	    begin
	      print_string "resulting cond: ";
	      KProblem.print_cond_eff ce;
	    end;
	  (match ce with { KProblem.ceff = e } ->
	     Mylist.fold
	       (fun acc2 l -> KProblem.SmallSetL.add l acc2)
	       acc e)
	end in
      let set_new_effects =
	Mylist.fold
	  collect_effects
	  KProblem.SmallSetL.empty temp_condeffs in
      let res = KProblem.SmallSetL.mylist_of set_new_effects in
	begin
(* 	  assert( Mylist.length temp_condeffs == 1 or Mylist.length temp_condeffs == 0 ); 
	  (\* OJO: si pasa, entonces crear menos Mylist *\) *)
	  if debug_get_effects then
	    begin
	      print_string "**** resulting: ";
	      KProblem.print_set_lit set_new_effects;
	    end;
	  res
	end

  (* get a new action *)
  let new_action acc_acts old_act  =
    if debug_k1 then
      print_endline ("*** To action "^GProblem.get_aname old_act);
    Mylist.add 
      { KProblem.aname = GProblem.get_aname old_act;
	prec = Mylist.map pkl (GProblem.get_prec old_act);
	eff = get_effects (GProblem.get_eff old_act);
	conds = Mylist.fold new_conds (Mylist.empty()) (GProblem.get_conds old_act);
      } acc_acts

  (* OJO CONTINGENT: reimplement with a general system of rules
     that subsummes contingent rules *)
  let get_a_merge acc g =
    let cond_merge m =
      let res = Mylist.empty () 
      in begin
	  List.iter 
	    (fun mi -> Mylist.add (pklt g mi) res)
	    m;
	  res;
	end in
    let eff_merge g = 
      if true then
	RamificationMutex.expand_k (pkl g)
      else
	let res = Mylist.empty ()
	in begin
	    Mylist.add (pkl g) res;
	    (* 	  Mylist.add (nknl g) res; (\* IS NOT NECESSARY *\) *)
	    res;
	  end in
    let get_cond_merge g acc m =
      Mylist.add
	{ KProblem.ccomment = merge_prefix;
	  cond = cond_merge m;
	  ceff = eff_merge g; } acc;
      acc in
    let mg = Uncertainty.extended_merges g
    in if mg == [] then
	acc
      else
	begin
	  Mylist.add
	    { KProblem.aname = merge_prefix^"_"^(GProblem.Lit.to_text g);
	      prec = Mylist.empty();
	      eff = Mylist.empty();
	      conds = List.fold_left (get_cond_merge g) (Mylist.empty()) mg;
	    } acc;
	  acc;
	end


  (* New initial state

     - collect tags
     - for each tag t:
     ---- s = lits(Unit prop(clauses+t))
     ---- for each if
     ------- if there is Ks/g, add to Init
  *)
  let get_new_init () =
    (* do not used I from P as it is being used in UnitPropL *)
    
    (* effectively add KL/t *)
    let gen_init_tag t l acc =
      if is_tagged l t then
	(* If L/t in atoms, add it to init *)
	begin
	  if false then
	    begin
	      print_string "Adding new lit to Init:";
	      KProblem.Lit.print_lit (pklt l t);
	      print_newline();
	    end;
	  KProblem.SmallSetL.add (pklt l t) acc;
	end
      else
	acc in
      
    (* For each tag c, run unit propagation to get other tags
       Assume clauses are in Prime Implicate form *)
    let proc_tag t acc =
      let lits = Tag_hits_star.do_f t in
	GProblem.SmallSetL.fold (gen_init_tag t) lits acc in

    let tags =  (* { t | KL/t }, including t = empty *)
      Timed.call "calculating tags in get_init"
	(fun () ->
	   (KProblem.Lit.A.tbl_fold
	      (fun _ atom acc ->
		 begin
		   KAtom.SparseSetT.add (tag_of_tagged atom) acc;
		   acc
		 end)
	      (KAtom.SparseSetT.empty ()))) in

    let simple_init = KProblem.SmallSetL.empty
    in 
      if false then
	begin
	  print_string "Atoms: ";
	  KProblem.print_tbl_atom ();
	  print_string "Tags: ";
	  KAtom.SparseSetT.iter 
	    (fun t -> KAtom.tag_print t; print_newline ())
	    tags;
	end;
      Timed.call "calculate init given returned atoms"
	(fun () ->
	   (*
	     { KL/t | t \in Init and t \implies L }
	   *)
	   list_map (fun l -> KProblem.Single l)
	     (KProblem.SmallSetL.list_of
		(KAtom.SparseSetT.fold proc_tag tags simple_init)))
	  
  (* new goal *)
  let get_new_goal lgoal =
    list_map
      (function
	   GProblem.Single g -> KProblem.Single (pkl g)
	 | GProblem.Clause _ -> raise (Failure "goal with clauses"))
      lgoal

  let get_k1 { GProblem.domain = ldomain; instance = linstance;
	       goal = lgoal; actions = lactions } =
    (* Resetting data structure and Cache *)
    Get_relevant_goals.reset();    
    Exists_in_tag_star.reset();
    Tag_hits_star.reset();
    
    if !GProblem.goals_and_precs == [] then
      GProblem.setup_goals_and_precs();
    
    let get_merges () =
      Timed.call "calculating merges"
	(fun () ->
	   List.fold_left get_a_merge (Mylist.empty()) (Uncertainty.mergeable())) in

    let new_actions () =
      let acts = get_merges() (* init actions *)
      in Mylist.iter (new_action acts) lactions;
	acts in
    let acts = Timed.call "calculating actions" new_actions in
    let new_init = Timed.call "calculating new init" (fun () -> get_new_init ())
    in { KProblem.domain = ldomain;
	 instance = linstance;
	 init = new_init;
	 goal = get_new_goal lgoal;
	 actions = acts }
end


(*****************************************************)
(**********  Adding non-det acts to KProblem    ******)
(*****************************************************)

module Nondeterminism_kproblem = struct
  let cache_oneof = ref( MapString.empty )
    
  let print_oneof () =
    begin
      print_endline "----------------------------------------";
      print_endline "Table of acts to oneof-lits:";
      MapString.iter
	(fun aname lits -> 
	   print_endline ("Act: "^aname^". Lits:");
	   GProblem.SparseSetL.iter GProblem.Lit.print_lit lits;
	   print_newline())
	!cache_oneof;
      print_endline "----------------------------------------";
    end

  let setup_oneof { KProblem.actions = actions }  =
    let proc_act { KProblem.aname = aname; conds = conds } =
      let s = GProblem.SparseSetL.empty () 
      in if SetString.mem aname !Nondeterminism.non_det_acts then
	  begin
	    Mylist.iter (* Iterate over cond-effects *)
	      (fun { KProblem.cond = cond } -> 
		 Mylist.iter (* For each cond ci*)
		   (fun ci ->
		      let (_,tag) = lit_and_tag_of_tagged (KProblem.Lit.atom_of_lit ci)
		      in KAtom.tag_iter (* For each tag t in ci *)
			   (fun t ->
                              if Nondeterminism.coincides t then
				GProblem.SparseSetL.add t s)
                           tag)
		   cond)
	      conds;
	    cache_oneof := MapString.add aname s !cache_oneof;
	  end
    in begin
	cache_oneof := MapString.empty;
	Mylist.iter proc_act actions;
	(* print_oneof (); *)
      end
	 
  let oneof_of_act aname =
    MapString.find aname !cache_oneof
      
  let nacts = ref([]) (* safe. reset in add_reset *)
  let reset_and_acts = ref([]) (* safe. reset in add_reset *)
  let add_reset { KProblem.actions = actions } =
    let proc_act = function { KProblem.aname = aname; prec = prec } as act ->
      let nprec = Mylist.empty() in
      let neff = Mylist.empty() in
      let nconds = Mylist.empty() in
      let c2e = KProblem.FastMapL.empty() in
      let dump_c2e () =
	KProblem.FastMapL.iter 
	  (fun ci s ->
	     let c = Mylist.empty() in
	     let e = Mylist.empty() in
	       begin
		 Mylist.add ci c;
		 KProblem.SparseSetL.iter
		   (fun ei -> 
		      (* cond-effect: l -> { klt } and -l -> { -klt } *)
		      Mylist.add ei e)
		   s;
		 Mylist.add { KProblem.cond = c; 
			      ceff = e; ccomment = ""} nconds;
	       end)
	  c2e
      in if SetString.mem aname !Nondeterminism.non_det_acts then
	  begin
	    (* convert preconditions and effect *)
	    Mylist.iter (fun p -> 
			   if Nondeterminism.coincides (lit_of_tagged_l p) then
			     begin
			       Mylist.add (neg_tagged_l p) nprec;
			       Mylist.add p neff;
			       Mylist.add (double_neg p) neff;
			     end) 
	      prec;
	    	      
	    (* create cond-effects *)
	    let add_ce is_pos l klt = 
	      let ci = if is_pos then (pkl l) else (nkl l) in
	      let ei = 
		if is_pos then KProblem.Lit.pos_atom klt 
		else KProblem.Lit.neg_atom klt
	      in begin (* simple cond-effect: l -> klt and -l -> -klt *)
		  let s =
		    (try 
		       KProblem.FastMapL.find c2e ci
		     with Not_found ->
		       let ns = KProblem.SparseSetL.empty()
		       in KProblem.FastMapL.add ci ns c2e; ns)
		  in KProblem.SparseSetL.add ei s
		end in
	    let proc_klt klt = (* for each KL/T with t in T *)
	      let (l,_) = lit_and_tag_of_tagged klt 
	      in if not (Nondeterminism.coincides l) then
		  (* don't delete oneof. Also other static atoms? *)
		  begin (* confirm or cancel KL/T, if KL *)
                    add_ce true l klt;
		    add_ce false l klt;
		  end in
	    let proc_t t = (* for each tag (of non-determinism) used in action  *)
	      let klt_set = Tag2lit.f t
	      in KProblem.SparseSetA.iter proc_klt klt_set
	    in GProblem.SparseSetL.iter proc_t (oneof_of_act aname);
	      
	    (* dump c2e *)
	    dump_c2e ();

	    (* add copy of action *)
	    let a = { KProblem.aname = (aname^"----reset"); prec = nprec; 
		      conds = nconds; eff = neff }
	    in begin
		nacts := a::!nacts;
		(* save act in case want to delete introduced preconds *)
		reset_and_acts := (a,act)::!reset_and_acts;
	      end
	  end
    in begin
	nacts := [];
	reset_and_acts := [];
	Mylist.iter proc_act actions;
	if false then
	  begin
	    print_endline "========================================";
	    print_endline "Actions added: ";
	    List.iter (KProblem.print_action (fun _ -> false)) !nacts;
	    print_endline "========================================";
	  end;
	List.iter (fun a -> Mylist.add a actions) !nacts;
      end

  (* if the reset actions do not reset any atom,   *)
  (* the switch is not necessary. Just delect those effects *)
  let del_reset () =
    let is_not_oneof_precond lit =
      not (Nondeterminism.coincides_txt (KProblem.Lit.to_text lit)) in
    let proc_act ({ KProblem.conds = reset_conds }, 
		  { KProblem.prec = prec }) =
      (* was cleaned by relevance, reachability, etc *)
      if Mylist.is_empty reset_conds then
	(* delete precondition containing 'ONEOF----', 
	   because reset won't be need anymore *)
	Mylist.filter is_not_oneof_precond prec;
    in List.iter proc_act !reset_and_acts
end

(*****************************************************)
(**************	Simplify cancellation	**************)
(*****************************************************)
  
module SimplifyCancel =
struct
  let debug_gen_cancellation = false

  let get_c_and_tag klit =
    if KProblem.Lit.lit_is_pos klit then
      raise (Failure "Transforming positive cond: internal error")
    else
      let atom = KProblem.Lit.atom_of_lit klit
      in lit_and_tag_of_tagged atom

  (* OJO: variable global totalmente innecesaria *)
  let tmpinit = ref(KProblem.FastSetL.empty ())  (* safe: reentrant *)

  (*
    OJO: compact with previous definition.
    general idea: certainty for l, under t
  *)
  let defined t l =
    let tl_pos = pklt l t in
    let tl_neg = pklt (GProblem.Lit.neg_lit l) t
    in (KProblem.FastSetL.mem !tmpinit tl_pos) ||
	 (KProblem.FastSetL.mem !tmpinit tl_neg)

  let calc_preserve_effect (cit: KProblem.Lit.t) =
    let (nci,t) = get_c_and_tag cit in
    let ci = GProblem.Lit.neg_lit nci
    in GProblem.SparseSetL.for_all
	 (defined t)
	 (Relevance.find_to_relevant ci)
  module Fun_preserve_effect =
  struct
    type t = bool
    let f lit =
      calc_preserve_effect lit
  end
  module Preserve_effect = KProblem.Lit.Cache(Fun_preserve_effect) (* safe for reentrant: reset in simplify_cancel *)
  let preserve_effect = Preserve_effect.do_f 

  let convert_cancellation = function 
      { KProblem.cond = ncond } as ce ->
	begin
	  Mylist.map_same double_neg ncond;
	  ce;
	end

  let preserve_or_non_cancellation = function
      { KProblem.ccomment = comment; cond = cond; } as condeffect ->
	assert( not (Mylist.is_empty cond) );
	let res = 
	  (comment != Translate.cancel_comment 
	      or not (Mylist.for_all preserve_effect cond)) 
	in begin
	    if not res && debug_gen_cancellation then
	      begin
		print_string "Deleting cancellation: ";
		KProblem.print_cond_eff condeffect;
	      end;
	    res
	  end
	     
  let convert_cond cond = 
    if preserve_or_non_cancellation cond then
      cond
    else
      convert_cancellation cond
	     
  let simplify_conds conds = 
    Mylist.map_same convert_cond conds

  (* if Translate.use_invariants it maybe better to use this version *)
  let simplify_conds_old conds = 
    Mylist.filter preserve_or_non_cancellation conds
      
  let simplify_action { KProblem.aname = aname; conds = conds } =
    if not (string_starts_with aname Translate.merge_prefix) then
      simplify_conds conds
	    
  let simplify_actions actions =
    Mylist.iter simplify_action actions

  let simplify_cancel { KProblem.init = linit; actions = lactions } =
    begin
      tmpinit := KProblem.fastset_lit_from_state linit;
      Preserve_effect.reset();
      (* calculate_tags lactions; *)
      simplify_actions lactions;
    end
end

(*****************************************************)
(********   Reasoning over static disjunctions *******)
(*****************************************************)

module Ramification =
struct
  let atoms_map_k_set = ref(KProblem.SmallMapL.empty) (* safe as is reset by setup *)
  let atoms_map_k = ref(KProblem.SmallMapL.empty) (* safe as is reset by setup *)
  let for_atom _ atom =
    let (l,tag) = lit_and_tag_of_tagged atom
    in if tag != KAtom.T.nil then
	let klatom = pkl l in
	let oldset =
	  try
	    KProblem.SmallMapL.find klatom !atoms_map_k_set
	  with Not_found ->
	    KProblem.SmallSetL.empty
	in atoms_map_k_set := KProblem.SmallMapL.add
	     klatom
	     (KProblem.SmallSetL.add (pklt l tag) oldset)
	     !atoms_map_k_set

  let setup () =
    begin
      atoms_map_k := KProblem.SmallMapL.empty;
      (* maybe redundant, dirty precaution at making code reentrant *)
      atoms_map_k_set := KProblem.SmallMapL.empty;
      KProblem.Lit.A.tbl_iter for_atom;
      KProblem.SmallMapL.iter
	(fun klatom klt_set ->
	   atoms_map_k :=
	     KProblem.SmallMapL.add klatom
	       (KProblem.SmallSetL.mylist_of klt_set)
	       !atoms_map_k)
	!atoms_map_k_set;
      atoms_map_k_set := KProblem.SmallMapL.empty
    end

  let expand_k_klt klit =
    let res = Mylist.empty ()
    in begin
	Mylist.add klit res;
	(try
	   Mylist.append_to 
	     res
	     (KProblem.SmallMapL.find klit !atoms_map_k)
	 with Not_found -> ());
	res
      end
end


(*****************************************************)
(********   Reasoning over static disjunctions *******)
(*****************************************************)

module StaticDisjunctions =
struct
  (*
    Preprocess:
    array of candidates
    array of bool (active) candidates
    relevant_clause: lit -> candidates_index containing lit
    Time: linear on clauses
  *)

  (* safe for reentrant as are reset by setup_clauses *)
  let clauses = ref( Array.make 0 GProblem.SmallSetL.empty )
  let active = ref( Array.make 0 true )
  let lit_in_clause = GProblem.SparseMapL.empty ()

  let get_lit_to_clause l =
    try
      GProblem.SparseMapL.find lit_in_clause l
    with Not_found -> []

  let assoc_lit_clause cindex l =
    let old_list = get_lit_to_clause l
    in GProblem.SparseMapL.add l (cindex::old_list) lit_in_clause

  let setup_clauses linit =
    let len = List.length linit in
    let n = ref(0) (* safe: reentrant *) in
    let setup_one_clause = function
	GProblem.Single _ -> ()
      | GProblem.Clause c ->
	  begin
	    !clauses.(!n) <- (GProblem.SmallSetL.of_list c);
	    List.iter (assoc_lit_clause !n) c;
	    incr n;
	  end
    in begin
	GProblem.SparseMapL.clear lit_in_clause;
	clauses := Array.make len GProblem.SmallSetL.empty;
	active := Array.make len true;
	List.iter setup_one_clause linit;
      end
	 
  (*
    For each e in eff E (uncond or cond)
    -- for each ei in E
    ----- For each active clause in relevant_clause(-ei)
    --------- // Should add other
    --------- if no ej in E also in clause:
    ------------ desactivate clause
  *)
  let proc_eff eff =
    let proc_clause cindex =
      if !active.(cindex) then
	let clause = !clauses.(cindex) in
	let in_clause ei =
	  GProblem.SmallSetL.mem ei clause
	in if not (Mylist.exists in_clause eff) then
	    !active.(cindex) <- false in
    let proc_one_eff e =
      let rel_clauses = get_lit_to_clause (GProblem.Lit.neg_lit e)
      in List.iter proc_clause rel_clauses
    in Mylist.iter proc_one_eff eff
	 
  let proc_cond { GProblem.ceff = effect } =
    proc_eff effect

  let proc_action { GProblem.eff = eff; conds = conds } =
    begin
      proc_eff eff;
      Mylist.iter proc_cond conds;
    end

  let get_static_clauses
      { GProblem.init = linit; actions = lactions } =
    let new_clauses = ref([]) (* safe: reentrant *)
    in begin
	setup_clauses linit;

	Mylist.iter proc_action lactions;

	for i = 0 to (Array.length !clauses)-1 do
	  if !active.(i) then
	    new_clauses := (!clauses.(i)):: !new_clauses;
	done;

	print_string "\nNumber of detected static clauses: ";
        print_int (List.length !new_clauses);
	print_newline();
	if false then
	  begin
	    print_endline "\nDetected static clauses: ";
            List.iter GProblem.print_set_lit !new_clauses;
	  end;

	!new_clauses;
      end

  let cond_of_clause acc clause =
    let cond_of_lit_clause xi acc =
      let get_a_cond xj acc =
	begin
	  if xi != xj then
	    Mylist.add (pknl xj) acc;
	  acc
	end
      in begin
	  Mylist.add 
	    { KProblem.ccomment = "reasoning over static clause";
              cond = GProblem.SmallSetL.fold get_a_cond clause (Mylist.empty());
	      (* OJO CONTINGENT: should ramify *)
              (* ceff = Ramification.expand_k_klt(pkl xi) } acc; *)
	      ceff = 
		Mylist.map_from_list 
		  (fun x -> x) 
		  (if Translate.use_invariants then 
		     (* In conformant is not necessary, but in contingent? *)
		     [pkl xi; nknl xi ]
		   else
		     [pkl xi]) }   acc;
	  acc;
	end
    in GProblem.SmallSetL.fold cond_of_lit_clause clause acc

  let static_merge_prefix = "MERGE_CLAUSES"
  let get_static_merge static_clauses =
    { KProblem.aname = static_merge_prefix;
      prec = Mylist.empty(); eff = Mylist.empty();
      conds = List.fold_left cond_of_clause (Mylist.empty ()) static_clauses; }
      
  let get_static_merges static_clauses { KProblem.actions = lactions } =
    let new_action = get_static_merge static_clauses
    in begin
	print_string "Generating Merge for ";
	print_int (List.length static_clauses);
	print_endline " static clauses";
	print_string "Num of cond-effects: ";
	print_int (List.fold_left 
		     (fun acc clause ->
			let l = GProblem.SmallSetL.cardinal clause
			in acc + (l*(l-1)))
		     0
		     static_clauses);
	print_endline "\nMaybe simplified away later on";
	Mylist.add new_action lactions;
      end
end
  
(*****************************************************)
(********   Action Compilation (AAAI-06)   ***********)
(*****************************************************)

(*
  In principle, these new cond-effects could be used
  to from the beginning to propagate tags, etc.
  It is sound not to use them.

  OJO: There was a bug in Act Compilation code in C++
  Verify this one is ok.
*)
module ActionCompilation =
struct

  (*
    For all actions a:
    -- For all conds Ci->Ei in a
    ----- Collect pairs l, -l st l \in Ci and -l in Ei
    ----- For all pair l, -l:
    -------- For all conds Cj->Ej in a
    ----------- collect conds A of condeff with l in Ej
    -------- For all possible S hitting each ALL conds A
    ----------- create condeff: -hit and (Ci sin l) -> K-l
  *)
  
  let debugit = false
    
  module OrderedPair =
  struct
    type t = GProblem.Lit.t * GProblem.condeffect
    let compare = compare (* WARNING: it assumes cond and effects are ordered.
			     Better to use set of list *)
(*     let compare ((l1,ce1):t) ((l2,ce2):t) = *)
(*       let v1 = GProblem.Lit.compare l1 l2 *)
(*       in if v1 != 0 then *)
(* 	  v1 *)
(* 	else *)
(* 	  let v2 = compare *)
(* 	    (GProblem.get_ccomment ce1) *)
(* 	    (GProblem.get_ccomment ce2) *)
(* 	  in if v2 != 0 then *)
(* 	      v2 *)
(* 	    else *)
(* 	      GProblem.Lit.compare_mylist_unsorted *)
(* 		(GProblem.get_cond ce1) *)
(* 		(GProblem.get_cond ce2) *)
  end
  module SetPair = Set.Make(OrderedPair)

  let effects_changed { GProblem.conds = conds } =
    let collect_from_condeff acc = function
	{ GProblem.cond = cond; ceff = ceff } as condeffect ->
	  (* assume cond and ceff are *short*, so O(n^2) algorithm *)
	  let collect_from_ceff acc e =
	    Mylist.fold
	      (fun acc c -> if c == GProblem.Lit.neg_lit e then
		 SetPair.add (e,condeffect) acc
	       else acc)
	      acc cond
	  in Mylist.fold collect_from_ceff acc ceff in
    let result = Mylist.fold collect_from_condeff SetPair.empty conds
    in if debugit then begin
	print_string " these literals changed:";
	SetPair.iter (function (e,_) -> GProblem.Lit.print_lit e) result;
      end;
      result

  let collect_conds e { GProblem.conds = conds } =
    let neg_e = GProblem.Lit.neg_lit e in
    let collect_from_condeff acc { GProblem.cond = cond; ceff = ceff } =
      (* if this condeffect deletes e *)
      if (Mylist.exists (fun e2 -> e2 == neg_e) ceff) then
	(* save its cond *)
	(Mylist.to_list cond)::acc
      else
	acc
    in let result = Mylist.fold collect_from_condeff [] conds
    in if debugit then begin
	print_endline "Collected conds: ";
	List.iter (fun comb -> GProblem.print_list_lit comb) result;
	print_endline "------------------";
      end;
      result
	
  let create_new_conds = function { GProblem.aname = aname } as act ->
    if debugit then begin
      print_string "====== Seeing action: ";
      print_endline aname;
    end;
    let changed = effects_changed act in
    let get_cond_for_e
	(* (lit changed, its causing condeffect) *)
	(e,{ GProblem.cond = cond })
	acc =
      if debugit then begin
	print_string "-------- Looking for change on:";
	GProblem.Lit.print_lit e;
	print_newline();
      end;
      let neg_e = GProblem.Lit.neg_lit e in
	(* all conditions but neg_e *)
      let new_cond = Mylist.filter_copy (fun c -> c != neg_e) cond in
      let new_kcond = Mylist.map pkl new_cond in
      let conds_delete = collect_conds e act in (* conds of condeffect that delete e *)
(*       let combs = combs_hits_all conds_delete in (\* combs of conds - sometimes are too many, expensive *\)  *)
      let combs = combs_hits_one conds_delete in (* combs of conds *)
      let create_new_cond new_kcond2 =
	List.iter (fun it -> Mylist.add it new_kcond) new_kcond2;
	{ KProblem.ccomment = "Act Compilation";
	  cond = new_kcond;
	  ceff = Ramification.expand_k_klt(pkl e) }
      in if debugit then begin
	  List.iter (fun comb -> print_string "Combination: "; GProblem.print_list_lit comb) combs;
	end;
	if combs == [] then
	  (create_new_cond [])::acc
	else
	  List.fold_left
	    (fun acc comb ->
	       let new_kcond2 = list_map pknl comb
	       in (create_new_cond new_kcond2)::acc)
	    acc combs
    in SetPair.fold get_cond_for_e changed []

  let get_aname_to_newconds { GProblem.actions = lactions } =
    let newconds_from_act acc = function
	{ GProblem.aname = aname } as act ->
	  MapString.add aname (create_new_conds act) acc
    in Mylist.fold newconds_from_act MapString.empty lactions

end


(*****************************************************)
(**************	Replace KL/t by KL     	**************)
(*****************************************************)

module DelKlt =
struct
  let tmpinit = ref(KProblem.FastSetL.empty ()) (* safe: reentrant *)
  let debug_del_klt = false
    
  let atoms_to_replace = KProblem.FastSetA.empty ()
  let do_replace_atom tagged =
    let (l,tag) = lit_and_tag_of_tagged tagged
    in if tag == KAtom.T.nil then
	false
      else
	let rel_to_l = Relevance.find_to_relevant l
	in GProblem.SparseSetL.for_all
	     (fun lp -> (* lp relevant to l *)
		(* K lp/t in Init => K lp in Init *)
		(is_tagged lp tag &&
		  not (KProblem.FastSetL.mem !tmpinit (pklt lp tag)))
		|| (KProblem.FastSetL.mem !tmpinit (pkl lp)))
	     rel_to_l

  let proc_atom _ atom =
    if do_replace_atom atom then
      begin
	if debug_del_klt then
	  begin
	    print_string "To replace: ";
	    KProblem.Lit.print_atom atom;
	    print_newline ();
	  end;
	KProblem.FastSetA.add atom atoms_to_replace
      end

  let replace_atom atom =
    if KProblem.FastSetA.mem atoms_to_replace atom then
      kl (lit_of_tagged atom)
    else
      atom

  let calc_replace_klit klit =
    if KProblem.Lit.lit_is_neg klit then
      KProblem.Lit.neg_atom (replace_atom (KProblem.Lit.atom_of_lit klit))
    else
      KProblem.Lit.pos_atom (replace_atom (KProblem.Lit.atom_of_lit klit))
  module Fun_replace_lit =
  struct
    type t = KProblem.Lit.t
    let f klit =
      calc_replace_klit klit
  end
  module Replace_lit = KProblem.Lit.Cache(Fun_replace_lit) (* safe for reentrant: reset in del_klt *)
  let replace_lit klit = Replace_lit.do_f klit

  let replace_list l =
    Mylist.iteri (fun i it -> Mylist.set l i (replace_lit it)) l
      
  let get_new_cond = function
      { KProblem.cond = cond; ceff = effect } ->
	replace_list cond;
	replace_list effect
      
  let get_new_action = function
      { KProblem.prec = prec; eff = eff; conds = conds } ->
	replace_list prec;
	replace_list eff;
	Mylist.iter get_new_cond conds

  let get_new_actions actions =
    Mylist.iter
      get_new_action
      actions

  let del_klt { KProblem.init = linit; actions = lactions } =
    begin
      KProblem.FastSetA.clear atoms_to_replace;
      Replace_lit.reset();
      tmpinit := KProblem.fastset_lit_from_state linit;
      if debug_del_klt then
	begin
	  print_string "Tmp_init:";
	  KProblem.FastSetL.iter
	    KProblem.Lit.print_lit
	    !tmpinit;
	  print_newline ();
	end;
      Timed.call "get atoms to replace"
	(fun () -> KProblem.Lit.A.tbl_iter proc_atom);
      if false then
	begin
	  print_string "\nAtoms to replace: ";
	  KProblem.print_fastset_atom atoms_to_replace;
	end;
      Timed.call "getting new actions" 
	(fun () -> get_new_actions lactions)
    end

end
  
exception Max_size
let size_of_tags_over_max final_max { KProblem.actions = lactions } =
  let proc_lit lit = 
    let v = String.length (KProblem.Lit.A.to_text (KProblem.Lit.atom_of_lit lit))
    in if v > final_max then
	raise Max_size in
  let proc_lst_lit lst =
    Mylist.iter proc_lit lst in
  let proc_cond { KProblem.cond = cond; ceff = ceff } =
    begin
      proc_lst_lit cond;
      proc_lst_lit ceff;
    end in
  let proc_act { KProblem.prec = prec; eff = eff; conds = conds } =
    begin
      proc_lst_lit prec;
      proc_lst_lit eff;
      Mylist.iter proc_cond conds;
    end
  in begin
      try
	Mylist.iter proc_act lactions;
	false
      with Max_size -> true
    end

(*****************************************************)
(**************      Tweaking PDDL       **************)
(*****************************************************)

(* Put actions in some order to minimize heuristics problem in FF :-S *)
let tweak_pddl = function { KProblem.actions = actions } as problem ->
  let starts_with s =
    function { KProblem.aname = aname } ->
      string_starts_with aname s  in
  let rev_actions = Mylist.rev actions in
    (*     let rev_actions = actions in *)
  let con_merge,sin_merge = Mylist.partition
    (starts_with Translate.merge_prefix) rev_actions in
  let con_merge_clauses,con_merge_normal = Mylist.partition
    (starts_with StaticDisjunctions.static_merge_prefix) con_merge 
  in KProblem.set_actions problem (Mylist.concat [sin_merge;con_merge_clauses;con_merge_normal])

(*****************************************************)
(**************	     Main    		**************)
(*****************************************************)


let print_stats () =
  let (nacts_g, nconds_g, ncondeff_lits_g) =
    GProblem.count !GProblem.problem
  and (nacts_k, nconds_k, ncondeff_lits_k) =
    KProblem.count !KProblem.problem
  in begin
      print_string "\nSTAT old-Actions: ";
      print_int nacts_g;
      print_string "\nSTAT old-Atoms: ";
      print_int (GProblem.FastSetA.length (GProblem.get_atoms_set !GProblem.problem));
      print_string "\nSTAT old-cond-effects: ";
      print_int nconds_g;
      print_string "\nSTAT old-condicionales: ";
      print_int ncondeff_lits_g;
      print_string "\nSTAT KP-NActions: ";
      print_int nacts_k;
      print_string "\nSTAT KP-NAtoms: ";
      print_int (KProblem.FastSetA.length (KProblem.get_atoms_set !KProblem.problem));
      print_string "\nSTAT KP-Ncond-effects: ";
      print_int nconds_k;
      print_string "\nSTAT KP-Numero_lit_en_efectos_condicionales: ";
      print_int ncondeff_lits_k;
      print_newline();
    end
      
let print_argv () =
  let rec proc_aux i =
    if i < Array.length Sys.argv then
      begin
	print_string "argv[";
	print_int i;
	print_string "] = ";
	print_endline Sys.argv.(i);
	proc_aux (i+1)
      end
  in proc_aux 0

let usage () =
  begin
    print_endline "Usage: ./cf2cs {options} <domain.pddl> <problem.pddl>";
    print_endline "where options a set of the following:";
    print_newline ();

    print_endline "-k0";
    print_endline "\t Force K_0: Only the empty tag";

    print_endline "-t0";
    print_endline "\t K1+K0, strategy for IPC5 and IPC6. if width is detected to be 1, add ONE merge";
    print_endline "\t\t for such satisfying clause. Otherwise, don't add any merge. NOT DEFAULT ANYMORE";

    print_endline "-ak1";
    print_endline "\t K1, optimized. DEFAULT: if width is detected to be 1, just add ONE merge for such satisfying clause";

    print_endline "-k1";
    print_endline "\t Force K1. Without optimization, compare with -ak1";

    print_endline "-s0";
    print_endline "\t For goal/precs with width > 1, gen merge with models of relevant clauses C_I(L)";

    print_endline "-fs0";
    print_endline "\t Force K_s0: For goal/precs with width > 1, gen same merge with models of INIT.";

    print_endline "-actcomp";
    print_endline "\t Do Action Compilation as in AAAI-06";

    print_endline "-static_disj";
    print_endline "\t Reason over preserved disjunctions";

    print_endline "-mog";
    print_endline "\t Create merges also for non preconditions or goals";

    print_endline "-s <prefix>";
    print_endline "\t to save pddls as <prefix>-d.pddl and <prefix>-p.pddl. Default: new";

    print_endline "-and";
    print_endline "\t starts another strategy to be tried, for example:";
    print_endline "\t\t -ak1 -static_disj -actcomp -and -s0 -static_disj -actcomp";
    print_endline "\t try to generate PDDL for -ak1, etc. If unreachable, skip to -s0, etc";
    print_endline "\t if reachable, generates PDDL and waits for";
    print_endline "\t signal USR1 for generating next PDDL or USR2 for finishing";
    print_endline "\t options accumulated in strategies:";
    print_endline "\t\t -k0, -ak1, -k1, -s0, -fs0, -mog, -actcomp, -static_disj";

    print_endline "-sp";
    print_endline "\t Kill parent process with SIGUSR1 when finish a PDDL (for being used by translator.py)";

    print_endline "-c";
    print_endline "\t Use cost 1 for actions, and 0 for merge";

    print_newline ();
    print_endline "-------------------------------------------------------------------------";
    print_endline "       Debugging options";

    print_endline "-v{0,1,2}";
    print_endline "\t verbosity level. Default: 1. Higher is more verbose";

    print_endline "-kconsistent";
    print_endline "\t Check consistency of classical problem K(P) (according to JAIR definition)";

    print_endline "-pconsistent";
    print_endline "\t Check consistency of conformant problem P (according to JAIR definition)";

    print_endline "-norm";
    print_endline "\t Normalize (lexicographic ordering) and save the pddls.";

    print_endline "-no_del_klt";
    print_endline "\t Don't use Del KL/t simplification. (Also accumulated in strategies. See -and)";

    print_endline "-sn";
    print_endline "\t Force use of short-unreadable names";

    print_endline "-ln";
    print_endline "\t Force use of long readable names";

    print_endline "-pln";
    print_endline "\t print table of meaning of short names";

    print_endline "-drel";
    print_endline "\t Debug Relevance, print table";

    print_endline "-dci";
    print_endline "\t Debug C_I(L)";

    print_endline "-dpddl";
    print_endline "\t Debug PDDL (print also intermediate PDDLs)";


    print_newline ();
    print_endline "-------------------------------------------------------------------------";
    print_endline "       For tracing executions (buggy, use 'validate' for classical plans)";
    print_endline "-trace <file-with-actions>";
    print_endline "\t for simulating the execution of actions in file";

    print_endline "-tdf";
    print_endline "\t Do Delete Free on simulation";

    print_endline "-topt";
    print_endline "\t optimistic: try-to-execute actions on simulation";

    print_endline "-tfp";
    print_endline "\t fixpoint: execute all actions until no change on state. Assume -topt";

    print_endline "-mac and -v";
    print_endline "\t Legacy options, for older versions of translator.py. Ignored.";


    exit(1);
  end

(* ALL are safe: set once *)
let option_do_test_consistency_p = ref(false)
let option_do_test_consistency_k = ref(false)
let option_do_trace = ref(false)
let option_trace_delfree = ref(false)
let option_trace_optimistic = ref(false)
let option_trace_fixpoint = ref(false)
let option_trace_filename = ref("")

let option_do_normalize = ref(false)

let option_signal_parent = ref(false)

let option_prefix = ref("new")
let option_use_cost = ref(false)
let option_long_names = ref(false)
let option_print_long_names = ref(false)
let use_short_names = ref(false)
let use_debug_pddls = ref(false)

(* Options for strategies *)
let default_use_old_t0 = false
let option_use_old_t0 = ref(default_use_old_t0)

let default_use_s0 = false
let option_use_s0 = ref(default_use_s0)

let default_use_also_real_k1 = true (* DEFAULT STRATEGY *)
let option_use_also_real_k1 = ref(default_use_also_real_k1)

let default_use_real_k1 = false
let option_use_real_k1 = ref(default_use_real_k1)

let default_use_force_s0 = false
let option_use_force_s0 = ref(default_use_force_s0)

let default_use_k0 = false
let option_use_k0 = ref(default_use_k0)

let default_do_merge_all = false
let option_do_merge_all = ref(default_do_merge_all)

let default_do_act_comp = false
let option_do_act_comp = ref(default_do_act_comp)

let default_do_static_disj = false
let option_do_static_disj = ref(default_do_static_disj)

let default_do_del_klt = true
let option_do_del_klt = ref(default_do_del_klt)

type strategy =
    { use_also_real_k1: bool;
      use_real_k1: bool;
      use_old_t0: bool;
      use_s0: bool;
      use_force_s0: bool;
      use_k0: bool;
      do_merge_all: bool;
      do_act_comp: bool;
      do_static_disj: bool;
      do_del_klt: bool;
    }
let strategy_lst = ref( [] : strategy list )
let consistency_options () =
  (* OJO: detect all combinations to be incompatible *)
  if (!option_use_s0  
      && !option_use_real_k1) then
    begin
      print_endline "Option -s0 and -k1 are incompatible";
      exit(1);
    end

let load_strategy () =
  begin
    consistency_options ();

    strategy_lst := 
      { use_also_real_k1 = !option_use_also_real_k1;
	use_real_k1 = !option_use_real_k1;
	use_old_t0 = !option_use_old_t0;
	use_s0 = !option_use_s0;
	use_force_s0 = !option_use_force_s0;
	use_k0 = !option_use_k0;
	do_merge_all = !option_do_merge_all;
	do_act_comp = !option_do_act_comp;
	do_static_disj = !option_do_static_disj;
	do_del_klt = !option_do_del_klt }
    ::!strategy_lst;
    
    (* Reset *)
    option_use_also_real_k1 := default_use_also_real_k1;
    option_use_real_k1 := default_use_real_k1;
    option_use_old_t0 := default_use_old_t0;
    option_use_s0 := default_use_s0;
    option_use_force_s0 := default_use_force_s0;
    option_use_k0 := default_use_k0;
    option_do_merge_all := default_do_merge_all;
    option_do_act_comp := default_do_act_comp;
    option_do_static_disj := default_do_static_disj;
    option_do_del_klt := default_do_del_klt;
  end
let finish_strategy () =
  begin
    load_strategy ();
    strategy_lst := List.rev !strategy_lst;
  end

let process_argv () =
  let rec proc_aux i =
    if i < Array.length Sys.argv then
      if Sys.argv.(i) = "-and" then
	begin
	  load_strategy();
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-pconsistent" then
	begin
	  option_do_test_consistency_p := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-kconsistent" then
	begin
	  option_do_test_consistency_k := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-trace" then
	begin
	  option_do_trace := true;
	  if i + 1 >= Array.length Sys.argv then
	    usage();
	  option_trace_filename :=  Sys.argv.(i+1);
	  proc_aux (i+2);
	end
      else if Sys.argv.(i) = "-tdf" then
	begin
	  option_trace_delfree := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-topt" then
	begin
	  option_trace_optimistic := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-tfp" then
	begin
	  option_trace_optimistic := true;
	  option_trace_fixpoint := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-s" then
	begin
	  if i + 1 >= Array.length Sys.argv then
	    usage();
	  option_prefix :=  Sys.argv.(i+1);
	  proc_aux (i+2);
	end
      else if Sys.argv.(i) = "-actcomp" then
	begin
	  option_do_act_comp := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-static_disj" then
	begin
	  option_do_static_disj := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-no_del_klt" then
	begin
	  option_do_del_klt := false;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-norm" then
	begin
	  option_do_normalize := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-s0" then
	begin
	  option_use_s0 := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-ak1" then
	begin
	  option_use_also_real_k1 := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-k1" then
	begin
	  option_use_real_k1 := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-fs0" then
	begin
	  option_use_force_s0 := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-k0" then
	begin
	  option_use_k0 := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-mog" then
	begin
	  option_do_merge_all := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-sn" then
	begin
	  use_short_names := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-ln" then
	begin
	  option_long_names := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-pln" then
	begin
	  option_print_long_names := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-c" then
	begin
	  option_use_cost := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-t0" then
	begin
	  option_use_old_t0 := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-mac" then
	proc_aux (i+1)
      else if Sys.argv.(i) = "-v" then
	proc_aux (i+2)
      else if Sys.argv.(i) = "-v0" then
	begin
	  Verbose.level :=  Verbose.level0;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-v1" then
	begin
	  Verbose.level :=  Verbose.level1;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-v2" then
	begin
	  Verbose.level :=  Verbose.level2;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-dpddl" then
	begin
	  use_debug_pddls := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-drel" then
	begin
	  Relevance.use_debug_relevance := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-dci" then
	begin
	  Uncertainty.use_debug_c_i_l := true;
	  proc_aux (i+1);
	end
      else if Sys.argv.(i) = "-sp" then
	begin
	  option_signal_parent := true;
	  proc_aux (i+1);
	end
      else if i + 2 < Array.length Sys.argv ||
	Sys.argv.(i).[0] = '-' then
	  usage()
  in begin
      proc_aux 1;
      finish_strategy();
    end

let try_trace_problem () =
  let get_action f =
    let l = input_line f in
      if string_starts_with l "(" then
	(* Remove '(' and ')' *)
	String.sub l 1 ((String.length l)-2) 
      else
	l in
  let rec file_to_list f acc =
    try
      file_to_list f ((get_action f)::acc)
    with End_of_file ->
      acc
  in if !option_do_trace then
      (* require each action un a separed line, without parens *)
      let file = open_in !option_trace_filename in
      let acts = List.filter (fun x -> x <> "") (List.rev(file_to_list file [])) in
	GProblem.simulate !option_trace_delfree !option_trace_optimistic !option_trace_fixpoint acts;
	exit(0)
    else
      begin
	print_string "WARNING: option not recognized -> ";
	print_endline Sys.argv.(1);
      end

let get_init_in_pi () =
  let all_clauses = List.fold_left GProblem.get_all_clause_list [] (GProblem.get_init()) in
  let new_clauses =
    (* if (not use_k0) && false then *)
    (* OJO: Don't caculate Prime Implicate for K0. Does it hurt so much? *)
    (* HEY! YEP! There was a case: ¿Para sortnet2 (sortnum)? *)
    (Timed.call "Getting clauses in PI form"
       (fun () ->
	  GProblem.get_in_pi_form all_clauses)) 
  in begin
      GProblem.set_init !GProblem.problem
	(List.fold_left GProblem.get_state_list_from_list [] new_clauses);
      if false then
	begin
	  let c = ref(0) in
	    print_string "\n========= Clauses BEFORE\n";
	    List.iter
	      (function clause ->
		 begin
		   print_int !c;
		   incr c;
		   print_string " -> ";
		   List.iter GProblem.Lit.print_lit clause;
		   print_newline()
		 end)
	      all_clauses;
	    print_string "========= End Clauses\n";
	    c:=0;
	    print_string "\n========= Clauses in PI form\n";
	    List.iter
	      (function clause ->
		 begin
		   print_int !c;
		   incr c;
		   print_string " -> ";
		   List.iter GProblem.Lit.print_lit clause;
		   print_newline()
		 end)
	      new_clauses;
	    print_string "========= End Clauses\n"
	end
      else
	let old_c = List.length all_clauses in
	let new_c = List.length new_clauses in
	  if new_c != old_c then
	    begin    
	      print_string "Enforcing Prime Implicate Form: ";
	      print_string "going from ";
	      print_int old_c;
	      print_string " to ";
	      print_int new_c;
	      print_endline " clauses";
	    end
    end

let initialize () =
  begin
    if not(!GProblem.problem_created) then
      usage();
    print_string "doing t0\n";
    process_argv();
    if !option_do_trace then
      try_trace_problem ();
    if !option_do_normalize then
      begin
	Normalize.normalize ();
	exit(0);
      end;
    get_init_in_pi ();
    ignore(GProblem.UnitPropL.setup_unit_prop !GProblem.problem);
  end

let nruns = ref 1
let translate_one option_use_also_real_k1 option_use_real_k1 
    option_use_old_t0 option_use_s0 option_use_force_s0 option_use_k0 option_do_merge_all 
    option_do_act_comp option_do_static_disj 
    option_do_del_klt option_long_names option_print_long_names option_prefix option_use_cost  =
  begin
    if !nruns > 1 || (List.length !strategy_lst > 1) then
      begin
	print_endline "--------------------------------------------------";
	print_endline "--------------------------------------------------";
	print_string "GENERATING PDDL NUMBER ";
	print_int !nruns;
	incr nruns;
	print_endline "\n--------------------------------------------------";
      end;

    if Nondeterminism.is_nondeterministic !GProblem.problem then
      print_endline ("Conformant problem is non-deterministic:\n"^
		       "Merging all literals, not only goals and precs")
    else
      print_endline "Conformant problem is deterministc";

    let option_do_merge_all = option_do_merge_all or 
      Nondeterminism.is_nondeterministic !GProblem.problem
    in 

      (* OJO OJO: do BEFORE of Prime Implicate enforcing *)
    Uncertainty.fix_init !GProblem.problem;
(*     if false then *)
    if !use_debug_pddls then
      GProblem.print_pddl "grounded0";
    (* Uncertainty.fix_goal !GProblem.problem; *)
    ignore(GProblem.clean_problem_by_reachability !GProblem.problem);
    if !use_debug_pddls then
      GProblem.print_pddl "grounded1";
    if !option_do_test_consistency_p then
      Timed.call "Consistency of conformant"
	(fun () -> GProblem.mutex_set !use_short_names !GProblem.problem);
(*  GProblem.assert_init !GProblem.problem; *)
 
    Timed.call "Relevance"
      (fun () -> Relevance.calc_relevance());
    
    Timed.call "Hitting"
      (fun () -> 
	 Uncertainty.setup_known_unknown();
	 Uncertainty.setup_hitting option_use_also_real_k1 option_use_real_k1 
	   option_use_old_t0 option_use_s0 option_use_force_s0 option_use_k0 option_do_merge_all);
    print_endline "\n=== Getting K Problem ===\n";
    
    Timed.call "RamificationMutex"
      (fun () -> RamificationMutex.setup ());

    KProblem.reset();
    Timed.call "Get k1 (supports)"
      (fun () -> KProblem.problem := Translate.get_k1 !GProblem.problem);
    
    disable_creation_klt ();

    (* Preparing for dealing with non-determinism *)
    Timed.call "Dealing with non-determinism (if needed)"
      (fun () -> 
	 Timed.call "Tag2lit.setup"
	   (fun () -> 
	      Tag2lit.setup ());
	 Nondeterminism.setup !GProblem.problem;
	 Nondeterminism_kproblem.setup_oneof !KProblem.problem;
	 (* add copy non-det actions, to reset some KL/t  *)
	 (*     and be able to use it again *)
	 Timed.call "Adding reset for non-determism (if needed)"
	   (fun () -> 
	      Nondeterminism_kproblem.add_reset !KProblem.problem));

    if !use_debug_pddls then
      KProblem.print_pddl "new-big";

    Timed.call "Simplify cancellation"
      (fun () -> SimplifyCancel.simplify_cancel !KProblem.problem);    

    Timed.call "Ramification"
      (fun () -> Ramification.setup ());
    
    if option_do_act_comp then
      Timed.call "Action Compilation"
	(fun () ->
	   KProblem.add_condeff_to_actions
	     (ActionCompilation.get_aname_to_newconds !GProblem.problem)
	     !KProblem.problem);
    
    if option_do_static_disj then
      begin
	let s = 
	  Timed.call "get Static clauses"
	    (fun () ->
	       StaticDisjunctions.get_static_clauses !GProblem.problem) in
	  Timed.call "Static clauses merges"
	    (fun () ->
	       StaticDisjunctions.get_static_merges s !KProblem.problem);
      end;
    
    if !use_debug_pddls then
      KProblem.print_pddl "new-big2";
    if option_do_del_klt then
      Timed.call "Del KL/t"
	(fun () -> DelKlt.del_klt !KProblem.problem);
    
(*  KProblem.report_size_problem "********* K-after *********" !KProblem.problem; *)
(*  if true then *)
    if Timed.call "Final reachability"
      (fun () -> KProblem.clean_problem_by_reachability !KProblem.problem) then
	begin
	  (* Reachable! *)
	  Timed.call "Compact all conditions k"
	    (fun () -> KProblem.problem := KProblem.compact_conds !KProblem.problem);
	  Timed.call "Empty cond-effects to effects" 
	    (fun () -> KProblem.empty_cond_to_effects !KProblem.problem);
	  Timed.call "Deleting reset for non-determism (if possible)"
	    (fun () ->
	       Nondeterminism_kproblem.del_reset ());
 	  Timed.call "Tweaking PDDL order of actions for FF" 
	    (fun () ->
	       tweak_pddl !KProblem.problem);
	  begin
	    let z = 70 (* magic number! *)
	    in if size_of_tags_over_max z !KProblem.problem && not option_long_names then
		begin
		  print_string "Warning: Tagged lits too large (>";
		  print_int z;
		  print_endline "c), using short (not human friendly) atoms in PDDL";
		  print_endline "use option -pln for getting meaning of atoms";
		  use_short_names := true;	     
		end;
	  end;	
	  if !option_do_test_consistency_k then
	    Timed.call "Consistency of classical"
	      (fun () -> 
		 KProblem.mutex_set !use_short_names !KProblem.problem);
	  if not option_long_names then
	    use_now_short_names := !use_short_names;
	  KProblem.print_pddl_cost option_prefix option_use_cost Translate.have_cost;
	  if option_print_long_names then
	    KProblem.print_short_long_table ();
	  use_now_short_names := false;
	  	  
	  print_stats ();
	  true
	end
    else
      false
  end

let handle_go_on n =
  begin
    print_string "Signal received (";
    print_int n;
    print_endline "): Go on";
  end

exception Finish_it
let keep_on = ref(true)
let handle_stop n = 
  begin
    print_string "Signal received (";
    print_int n;
    print_endline "): Stop";
    keep_on := false;
    raise Finish_it
  end

let debug_signals = false
let pause_signals () =
  if debug_signals then
    print_endline "pause_signals";
  ignore(Unix.sigprocmask Unix.SIG_SETMASK [Sys.sigusr1; Sys.sigusr1]);
  if debug_signals then
    print_endline "pause_signals done"
      
let cont_signals () =
  if debug_signals then
    print_endline "cont_signals";
  Unix.sigsuspend [];
  (*   Unix.sigsuspend (Unix.sigpending ()); *)
  if debug_signals then
    print_endline "cont_signals done"

let install_handles () =
  begin
    Sys.set_signal Sys.sigusr1 (Sys.Signal_handle handle_go_on);
    Sys.set_signal Sys.sigusr2 (Sys.Signal_handle handle_stop);
  end

let left_iter = ref 2000
let orig_problem = ref( GProblem.empty_problem () )

(* Called many times, sometimes returned 1 :-S. Interrupted and corrupted? This is ok, anyway *)
let ppid = Unix.getppid() 

let rec iter_strategies = function
    [] -> begin
      if Nondeterminism.is_nondeterministic !GProblem.problem then
	if !left_iter <= 0 then
	  begin
	    print_endline "------------------------------";
	    print_endline "Max number of iterations for Non-deterministic reached.";
	  end
	else
	  begin
	    decr left_iter;
	    orig_problem := Nondeterminism.add_copy !orig_problem;
	    GProblem.problem := GProblem.copy_problem !orig_problem;
	    (* Process all strategies again, with new problem *)
	    print_endline "------------------------------";
	    print_endline "Non-deterministic problem. Will generate new PDDLs until signal USR2 is received, or killed";
	    iter_strategies !strategy_lst;
	  end
      else
	begin
	  print_endline "------------------------------";
	  print_endline "Finishing cf2cs";
	end
    end
  | s::r ->
      print_endline "************************************************************";
      print_endline "************************************************************";
      print_endline "Running configuration as follows:";
      if s.use_also_real_k1 then
	print_endline "* use_also_real_k1"
      else
	print_endline "* NOT use_also_real_k1";
      if s.use_real_k1 then
	print_endline "* use_real_k1"
      else
	print_endline "* NOT use_real_k1";
      if s.use_old_t0 then
	print_endline "* use_old_t0"
      else
	print_endline "* NOT use_old_t0";
      if s.use_s0 then
	print_endline "* use_s0"
      else
	print_endline "* NOT use_s0";
      if s.use_force_s0 then
	print_endline "* use_force_s0"
      else
	print_endline "* NOT use_force_s0";
      if s.use_k0 then
	print_endline "* use_k0"
      else
	print_endline "* NOT use_k0";
      if s.do_merge_all then
	print_endline "* do_merge_all"
      else
	print_endline "* NOT do_merge_all";
      if s.do_act_comp then
	print_endline "* do_act_comp"
      else
	print_endline "* NOT do_act_comp";
      if s.do_static_disj then
	print_endline "* do_static_disj"
      else
	print_endline "* NOT do_static_disj";
      if s.do_del_klt
      then
	print_endline "* do_del_klt"
      else
	print_endline "* NOT do_del_klt";
      let reachable = 
	Timed.call "translate_one"
	  (fun () ->
	     translate_one
	       s.use_also_real_k1 s.use_real_k1 s.use_old_t0 s.use_s0 s.use_force_s0 
	       s.use_k0 s.do_merge_all s.do_act_comp s.do_static_disj s.do_del_klt
	       !option_long_names !option_print_long_names !option_prefix !option_use_cost)
      in
	begin
	  if not reachable then
	    begin
	      print_endline "\n\nGoal is not reachable for this configuration. Continuing...\n";
	    end;
	  pause_signals ();
	  if reachable && !option_signal_parent then
	    begin
	      print_endline ("Letting know procees "^
			       (string_of_int ppid)^
			       " that I've finished.");
	      Unix.kill ppid Sys.sigusr1;
	    end;
	  if reachable && (r != [] || Nondeterminism.is_nondeterministic !GProblem.problem) then
	    begin
	      print_endline "\n\nPDDL generated. Waiting for signal for generating other PDDLs (USR1) or finishing (USR2).";
	      cont_signals ();
	      (* Unix.pause(); *)
	    end;
	  if !keep_on then
	    iter_strategies r;
	end

let do_t0 () =
  begin
    Timed.call "ALL"
      (fun () ->
	 initialize ();
	 install_handles();
	 orig_problem := GProblem.copy_problem !GProblem.problem;
	 try
	   iter_strategies !strategy_lst
	 with Finish_it -> ();
	 if !use_debug_pddls then
	   GProblem.print_pddl "grounded3";
      );
    flush !mystdout;
  end

(*****************************************************)
(********** Publishing for using from C/C++ **********)
(*****************************************************)

let _ = Callback.register "init_problem" GProblem.init_problem
and _ = Callback.register "fill_atom" GProblem.fill_atom
and _ = Callback.register "create_cond_effect" GProblem.create_cond_effect
and _ = Callback.register "create_action" GProblem.create_action
and _ = Callback.register "create_problem" GProblem.create_problem
and _ = Callback.register "add_init_lit" GProblem.add_init_lit
and _ = Callback.register "add_init_clause" GProblem.add_init_clause
and _ = Callback.register "add_goal_lit" GProblem.add_goal_lit
and _ = Callback.register "add_goal_clause" GProblem.add_goal_clause
and _ = Callback.register "do_t0" do_t0

let _ = print_endline "OCaml code, loaded!"


       
(** We represent nodes in the CFG simply as ints *)
type node = int

(** We represent CFGs in adjacency-list format, 
    using our good friend [Symtab.t]. 

    [Convention: Node 0 is the start node]
    --------------------------------------
    By convention, we call node 0 the start node.    

    [Invariant: Canonical labeling]
    -------------------------------
    As an invariant, we ensure that the nodes in any 
    CFG are always labeled: 
      0, 1, ..., #nodes-1
    (with no gaps). In particular, all table types 
    below, including: 
      - cfg
      - preds
      - dominators
    ensure that 
      [dom(tbl) = {0, 1, ..., #nodes-1}].

    Also, for this example we've updated the Symtab 
    module slightly, to use integer rather than 
    string keys. *)
type 'a table = ('a list) Symtab.t
	      
type cfg = node table

(** General-purpose Table Operations 
    -------------------------------- *)

(** Return the entries associated with key [x] (if any) *)
let entries_of (tbl : 'a table) (x : int) : 'a list =
  match Symtab.get x tbl with
  | None -> []
  | Some l -> l

(** Let [(k, l) = entry].
    Update [tbl] to map [k] to [l] (overwriting the 
    previous entry, if any) *)
let update_entry
	  (tbl : 'a table)
	  (entry : (int * 'a list)) : 'a table =
  let (k, l) = entry in
  Symtab.set k l tbl

(** Add a series of entries, of the form 
      (k1, v1) ::
      (k2, v2) ::
      ...
      (kN, vN) *)	     
let rec add_entries
	  (tbl : 'a table)
	  (entries : (int * 'a) list) : 'a table =
  match entries with
  | [] -> tbl
  | (k, v) :: entries' ->
     let l = entries_of tbl k in
     let tbl' = update_entry tbl (k, v :: l) in
     add_entries tbl' entries'

(** The number of nodes in [tbl] is equal to the number
    of entries in the underlying symbol table. 

    We extend the Symtab interface to support this 
    size operation. *)			  
let size (tbl : 'a table) : int = Symtab.size tbl
		 
(** Print the table [tbl]. 
    Assumes, by [Invariant: Canonical Labeling], 
    that every node that appears in [tbl] is in 
    [dom(tbl)]. *)		 
let print_table (tbl : 'a table) : unit =
  let print_entries k =
    let l = entries_of tbl k in
    BatList.iter (fun x -> print_int x; print_string " ") l;
    print_string "\n"
  in 
  let rec go n =
    if n < 0 then flush stdout
    else (print_int n;
          print_string " -> ";
	  print_entries n;
	  go (n-1))
  in go (size tbl - 1)

(** END General-purpose Table Operations
    ------------------------------------ *)

(** The successors of [x], if any. 

    Recall that [y] is a successor of [x] if there's
    a directed edge [x->y]. *)		       
let succs (g : cfg) (x : node) : node list =
  entries_of g x
		
(** Is there a directed edge from [x] to [y]? *)		       
let edge (g : cfg) (x : node) (y : node) : bool =
  BatList.mem y (succs g x)

(** The predecessors table has the exact same 
    representation as [cfg] but interpreted thus:

    A map from the nodes in the graph to their  
    predecessors (if any) 

    Recall that [x] is a predecessor of [y] if 
    the graph contains a directed edge [x->y]. *)
type preds = node table

let preds_of (p : preds) (x : node) : node list =
  entries_of p x

(** Construct the predecessor table for [g] *)	     
let build_preds (g : cfg) : preds =
  (* Add n to pred table entry for each y \in l *)  
  let add_preds (acc : preds) (n : node) (l : node list) =
    add_entries acc (BatList.map (fun y -> (y, n)) l)
  in
  (* This function can also be expressed as a "fold": *)
  let rec go (acc : preds) (n : node) : preds =
    if n < 0 then acc
    (* n is pred of each y \in succs g n *)
    else go (add_preds acc n (succs g n)) (n-1)
  in
  (* To enforce [Invariant: Canonical Labeling], 
     the initial preds table adds an empty entry 
     for each node in {0, 1, ..., #nodes(g)-1}. *)
  let rec init_preds (acc : preds) (n : int) =
    if n < 0 then acc
    else init_preds (Symtab.set n [] acc) (n-1)
  in
  go (init_preds (Symtab.create ()) (size g - 1))
     (size g - 1)
			  
(** The dominator table has the exact same 
    representation as [cfg] but interpreted thus:

    A map from the nodes in the graph to the 
    nodes that dominate it (if any) *)
type dominators = node table

let doms_of (d : dominators) (x : node) : node list =
  entries_of d x
	     
let build_dominators (g : cfg) : dominators =
  (* First, construct the preds table for g *)
  let p = build_preds g in

  (* go: The main function implementing the dominator 
     equations *)
  let rec go (acc : dominators) (n : node) : dominators =
    (* Define a little inner function that calculates 
       the intersection of the node lists in [ll], for 
       use below *)
    let intersect (ll : (node list) list) : node list =
      BatList.filter
	(fun k -> BatList.for_all (fun l -> BatList.mem k l) ll)    
	(BatList.init (size g) (fun j -> j))
    in
    if n < 0 then acc
    else if n = 0 then acc
    else let pred_doms = BatList.map (doms_of acc) (preds_of p n) in
         go (update_entry acc (n, n :: intersect pred_doms)) (n-1)
  in

  (* gfp: repeat go until we reach a (greatest) fixed point *)
  let rec gfp (acc : dominators) : dominators =
    let acc' = go acc (size g - 1) in
    print_string "round\n"; 
    (if acc = acc' then acc
     else (print_table acc'; flush stdout; gfp acc'))
  in

  (* The rest of the code just sets up our input table ... *)
  let init_doms =
    let entries_for (k : int) =
      BatList.init (size g) (fun i -> (k, i))
    in 
    update_entry 
      (add_entries
	 (Symtab.create ())
	 (BatList.flatten
	    (BatList.init (size g) (fun k -> entries_for k))))
      (0, [0])

  (* ... and calls [gfp] *)      
  in print_string "init_doms\n";
     print_table init_doms;
     gfp init_doms

let test (name : string) (g : cfg) : unit =
  print_string name; print_string "\n";
  print_table g;
  print_string "preds\n";
  print_table (build_preds g);
  print_string "dominators\n";
  print_table (build_dominators g)
;;

let cfg1 =
  add_entries
    (Symtab.create ())
    (* To satisfy [Invariant: Canonical Labeling], 
       we add a self-edge for 3 *)
    [(0, 1); (1, 2); (2, 3); (3, 3)];;

test "cfg1" cfg1;;
  
let cfg2 =
  add_entries
    (Symtab.create ())
    [(0, 1); (0, 2); (2, 3); (0, 3); (3, 2); (1, 1)];;

test "\ncfg2" cfg2;;

let cfg3 =
  add_entries
    (Symtab.create ())
    [(0, 1); (1, 2); (2, 1); (1, 3); (3, 1);
     (3, 4); (3, 5); (4, 6); (5, 6); (6, 6)];;

test "\ncfg3" cfg3;;
  
  
	       
				 

				 


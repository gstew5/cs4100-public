open Batteries

type 'a t = (string, 'a) BatMap.t
		      
let create _ = BatMap.empty

let get k m =
  try Some (BatMap.find k m)
  with Not_found -> None

let set k v m = BatMap.add k v m

module type Set_t = sig
  type 'a t
  val empty : 'a t
  val sigleton : 'a-> 'a t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val mem : 'a -> 'a t -> bool
end

module Set : Set_t = struct
  type 'a t = 'a list
  let empty = []
  let sigleton element = [element]
  let union set1 set2 = set1 @ set2
  let inter set1 set2 = 
    List.fold_left (fun lst element ->
        if List.mem element set2 then element :: lst else lst)
      [] set1
  let diff set1 set2 = 
    List.fold_left (fun lst element ->
        if List.mem element set2 then lst else element :: lst)
      [] set1
  let mem element set = List.mem element set
end

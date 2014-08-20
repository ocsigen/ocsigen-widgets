{shared{
  include module type of List

  (** [find_remove f l] takes the first value from the list returning true, and
        returns it (as the 'a). May raise Not_found *)
  val find_remove : ('a -> bool) -> 'a list -> 'a * 'a list

  (** [find_remove k l] takes the first value from the list equal to k, and
        returns it (as the 'a). May raise Not_found *)
  val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list

  (** Remove duplicates in a sorted list *)
  val uniq : 'a list -> 'a list
}}

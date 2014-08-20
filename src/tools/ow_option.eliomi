(** Manipulation on option values. *)

{shared{
  (** [map f v] returns the application of f on the option v if v is not None, or None. *)
  val map : ('a -> 'b) -> 'a option -> 'b option

  (** [map_lwt] is like [map] but returns a Lwt value. *)
  val map_lwt : ('a -> 'b Lwt.t) -> 'a option -> 'b option Lwt.t

  (** [iter f v] apply f to v if v is not None. *)
  val iter : ('a -> unit) -> 'a option -> unit

  (** [iter_lwt] is like [iter] but returns a Lwt value. *)
  val iter_lwt : ('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

  (** [lwt_map t f g] If the thread has returned a value v, returns (f v). Else, returns g () *)
  val lwt_map : 'a Lwt.t -> ('a -> 'b) -> (unit -> 'b) -> 'b
}}

{client{
  (** De-optize the value. May raise "of_opt"*)
  val of_opt : 'a Js.Opt.t -> 'a
}}

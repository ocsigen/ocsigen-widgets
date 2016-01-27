(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Charly Chevalier
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Manipulation on option values. *)

[%%shared.start]
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


[%%client.start]
  (** De-optize the value. May raise "of_opt"*)
  val of_opt : 'a Js.Opt.t -> 'a


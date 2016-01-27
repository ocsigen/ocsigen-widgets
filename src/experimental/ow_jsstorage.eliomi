(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Arnaud Parant
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

[%%client.start]
  (** Allow to store informations on the client.
      @author : Arnaud Parant
  *)

  (** Generic signature for storage on the client side. *)
  module type JSSTORAGE =
    sig

      type t
      type 'a key

      (** If storage does not existe,
          It launch failwith "Storage is not available" *)
      val get : unit -> t

      val length : t -> int

      (** [create_key name json_type] *)
      val create_key : string -> 'a Deriving_Json.t -> 'a key

      (** [get_name_key storage index] *)
      val get_name_key : t -> int -> string option

      val get_item : t -> 'a key -> 'a option

      (** [get_noopt_item storage key json_type default_value] *)
      val get_noopt_item : t -> 'a key -> 'a -> 'a

      val set_item : t -> 'a key -> 'a -> unit

      val remove_item : t -> 'a key -> unit

      val clear : t -> unit

    end

  module SessionStorage : JSSTORAGE

  module LocalStorage : JSSTORAGE


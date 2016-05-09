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

[%%shared.start]
  type t'


[%%client.start]
  class type item = object
    inherit Dom_html.element

    method enable : unit Js.meth
    method disable : unit Js.meth
  end

  class type item' = object
    inherit item

    method _enable : (#item Js.t, unit) Js.meth_callback Js.prop
    method _disable : (#item Js.t, unit) Js.meth_callback Js.prop
  end

  type t

  val set : ?at_least_one : bool -> unit -> t

  val enable : set:t -> #item Js.t -> unit
  val disable : set:t -> #item Js.t -> unit

  val ctor :
       enable : (#item Js.t -> unit)
    -> disable : (#item Js.t -> unit)
    -> #item' Js.t
    -> item Js.t


[%%client.start]
  val set : ?at_least_one:bool -> unit -> t
  val of_server_set : t' -> t
  val to_server_set : t -> t'

(* Eliom-base-app
 * http://www.ocsigen.org/eliom-base-app
 *
 * Copyright (C) 2014
 *      Vincent Balat
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

(** Wait spinner image. *)

{shared{
module D : sig

  (** Display spinner with DOM semantics *)
  val spinner :
    ?class_:Html5_types.nmtoken list ->
    unit -> [> Html5_types.i ] Eliom_content.Html5.D.elt

end

module F : sig

  (** Display spinner with functional semantics *)
  val spinner :
    ?class_:Html5_types.nmtoken list ->
    unit -> [> Html5_types.i ] Eliom_content.Html5.F.elt

end

(** On client side, [with_spinner th] returns immediately a spinner
    while Lwt thread [th] is not finished, that will automatically
    be replaced by the result of [th] when finished.

    On server side, it will wait for [th] to be finished before returning
    its result (and never display a spinner).
*)
val with_spinner :
  ?a:[< Html5_types.div_attrib ] Eliom_content.Html5.F.attrib list ->
  ?fail:(unit -> ([> Html5_types.div ] as 'a) Eliom_content.Html5.F.elt Lwt.t) ->
  'a Eliom_content.Html5.F.elt Lwt.t ->
  'a Eliom_content.Html5.F.elt Lwt.t

 }}

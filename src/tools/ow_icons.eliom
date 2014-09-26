(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014
 *      Séverine Maingaud
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

(** This file defines icons for web applications using
    Awesome font family.
    Add the appropriate CSS and fonts to use them.
*)

{shared{
module Make(A : module type of Eliom_content.Html5.F) = struct

  let icon classes ?(class_=[]) () =
    A.i ~a:[A.a_class ("fa"::classes@class_)] []

  let user = icon ["fa-user"; "fa-fw"]
  let plus = icon ["fa-plus"; "fa-fw"]
  let envelope = icon ["fa-envelope"; "fa-fw" ]
  let logout = icon ["fa-logout"; "fa-fw"]
  let spinner = icon ["fa-spinner"; "fa-spin"; "fa-fw"]
  let file = icon ["fa-file"; "fa-fw"]
  let download = icon ["fa-cloud-download"; "fa-fw"]
  let share = icon ["fa-share"; "fa-fw"]
  let shutdown = icon ["fa-sign-out"; "fa-fw"]
  let config = icon ["fa-gear"]
  let signout = icon ["fa-signout"]
  let close = icon ["fa-close"]
  let question = icon ["fa-question"]

end

module F = Make(Eliom_content.Html5.F)
module D = Make(Eliom_content.Html5.D)

}}

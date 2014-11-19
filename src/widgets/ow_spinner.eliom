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

{shared{
open Eliom_content
}}

(** Wait spinner image. *)
{shared{
module D = struct
   let spinner = Ow_icons.D.spinner
end
module F = struct
   let spinner = Ow_icons.F.spinner
end
 }}

{server{
   let with_spinner
       ?a ?(fail=fun () -> Lwt.return (Html5.F.div ?a [Ow_icons.F.question ()]))
       thread =
     try_lwt thread with _ -> fail ()
 }}

{client{
   let with_spinner
       ?a ?(fail=fun () -> Lwt.return (Html5.F.div ?a [Ow_icons.F.question ()]))
       thread =
     match Lwt.state thread with
     | Lwt.Return v -> Lwt.return v
     | Lwt.Sleep ->
       let d = Html5.D.div ?a [Ow_icons.F.spinner ()] in
       Lwt.async
         (fun () ->
            lwt v = try_lwt thread with _ -> fail () in
            Eliom_content.Html5.Manip.replaceSelf d v; Lwt.return ());
       Lwt.return d
     | Lwt.Fail _ -> fail ()
 }}

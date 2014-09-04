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

{client{
  open Dom
  open Dom_html
  (* time *)

  let get_timestamp () =
    let date = jsnew Js.date_now () in
    Js.to_float (date##getTime ())

  let as_dom_elt elt f =
    elt##style##visibility <- Js.string "hidden";
    appendChild document##body elt;
    let ret = f elt in
    removeChild document##body elt;
    elt##style##visibility <- Js.string "visible";
    ret

  (* ?parent is a function which returns the parent of the close button.
     ?close is a function called when closing the parent of the close button.
     *)
  let closeable
      ?(get_parent : (#element Js.t -> Dom.node Js.t) option)
      ?(on_close : (Dom.node Js.t -> unit) option)
      (elt : #element Js.t) =
    elt##classList##add(Js.string "ojw_close");
    (* Function wrapper, if there is no close parameter, so we delete the parent
       of the close button from the document. *)
    let close_parent p =
      match on_close with
      | None ->
          Js.Opt.iter (p##parentNode)
            (fun super_parent ->
               removeChild super_parent p)
      | Some on_close -> on_close p
    in
    (* If there is no parent paramter, we use the parent node of the close
       button. *)
    match get_parent with
    | None ->
        (fun () ->
           Js.Opt.iter (elt##parentNode)
             (fun p -> close_parent p))
    | Some get_parent ->
        (fun () ->
           close_parent (get_parent elt))


  let closeable_by_click ?get_parent ?on_close elt =
    let f = closeable ?get_parent ?on_close elt in
    Lwt.async (fun () ->
        Lwt_js_events.clicks elt
          (fun _ _ ->
             f ();
             Lwt.return ()))
}}

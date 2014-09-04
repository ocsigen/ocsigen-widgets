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

{shared{
  open Ow_pervasives

  (* FIXME
   * It would be MUCH better to take the value as first parameter
   * and the function to apply as second. Like Js.Opt
   *
   * *)

  let map f v =
    match v with
      | None -> None
      | Some a -> Some (f a)

  let map_lwt f v =
    match v with
      | None -> Lwt.return None
      | Some a -> Lwt.bind (f a) (fun r -> Lwt.return (Some r))

  let iter f v =
    match v with
      | None -> ()
      | Some a -> f a

  let iter_lwt f v =
    match v with
      | None -> Lwt.return ()
      | Some a -> f a

  let lwt_map t f g =
    match Lwt.state t with
      | Lwt.Return v -> f v
      | _ -> g ()
}}

(* FIXME
 * Should be of_jsOpt or of_js_opt ?
 *
 * *)
{client{
  let of_opt elt =
    Js.Opt.case elt (fun () -> failwith "of_opt") id
}}

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
  open Eliom_content.Html5
  open Html5_types

[%%client.start]
  open Dom_html
  open Dom


[%%client.start]
  (** Popups are a special kind of [alerts] that are automatically showed in
      the middle of the screen. *)

  (** {2 Helpers} *)

  (** Internal background is shown when a popup is triggered. *)
  val show_background : unit -> unit

  (** Internal background id hidden when a popup is triggered. *)
  val hide_background : unit -> unit

  (** {2 Construction functions} *)

  (** Specialization of a [Ow_alert.alert] that shows a transparent
      background if [with_background] is set to [true]. *)
  val popup :
     ?show:bool
  -> ?allow_outer_clicks:bool
  -> ?with_background:bool
  -> 'a elt
  -> 'a elt

  (** Specialization of a [Ow_alert.dyn_alert] that shows a transparent
      background if [with_background] is set to [true].  *)
  val dyn_popup :
     ?show:bool
  -> ?allow_outer_clicks:bool
  -> ?with_background:bool
  -> 'a elt
  -> ('a elt -> 'a elt list Lwt.t)
  -> 'a elt

  (** Alias to [Ow_alert_sigs.closeable_by_click] *)
  val closeable_by_click : 'a elt -> 'a elt

  (** {2 Conversion functions} *)

  (** Tests if an element is an alert or not and returns it as a [popup]
      instance. *)
  val to_popup : 'a elt -> Ow_alert.alert Js.t

  (** Tests if an element is an alert or not and returns it as a [dyn_popup]
      instance. *)
  val to_dyn_popup : 'a elt -> Ow_alert.dyn_alert Js.t


[%%server.start]
  val closeable_by_click :
     'a elt
  -> 'a elt

  val popup :
     ?show:bool
  -> ?allow_outer_clicks:bool
  -> ?with_background:bool
  -> 'a elt
  -> 'a elt

  val dyn_popup :
     ?show:bool
  -> ?allow_outer_clicks:bool
  -> ?with_background:bool
  -> 'a elt
  -> ('a, _) Ow_alert.dyn_alert_fun' Eliom_pervasives.client_value
  -> 'a elt

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

[%%client.start]
  (** Various tools related to events. *)

  (** {3 Position and Coordination } *)

  type position_type = Client | Screen | Page
  type touch_type = All_touches | Target_touches | Changed_touches

  (** Take two coordinates and return true if they are equal *)
  val cmp_coord : (int * int) -> (int * int) -> bool
  (* TODOC this is an equality function, not a compare ("cmp") one. Also, not sure to see the point since equality and comparaison are polymorphic in ocaml (and are good enough for most purposes).*)

  (** {5 Mouse} *)

  (** @param p_type [Client] by default. *)
  val get_mouse_ev_coord :
    ?p_type:position_type ->
    Dom_html.mouseEvent Js.t ->
    int * int

  (** Similar to [get_mouse_ev_coord].
      Calculate the position of the mouse inside a given element.
  *)
  (* TODOC Not sure if correct, but already better than the original description. *)
  val get_local_mouse_ev_coord :
    #Dom_html.element Js.t ->
    ?p_type:position_type ->
    Dom_html.mouseEvent Js.t ->
    int * int

  (** {5 Touch} *)

  (** @param p_type [Client] by default. *)
  val get_touch_coord :
    ?p_type:position_type ->
    Dom_html.touch Js.t ->
    int * int

  (** Similar to [get_touch_coord].
      Take the index of touch (for multi-touch) as argument.
      @param t_type [All_touches] by default. *)
  val get_touch_ev_coord :
    ?t_type:touch_type ->
    int ->
    ?p_type:position_type ->
    Dom_html.touchEvent Js.t -> int * int
  (** Similar to [get_touch_ev_coord].
      Calculate the position of the touch inside a given element.
      Take the index of touch (for multi-touch) as argument. *)
  val get_local_touch_ev_coord :
    #Dom_html.element Js.t ->
    ?t_type:touch_type ->
    int ->
    ?p_type:position_type ->
    Dom_html.touchEvent Js.t ->
    int * int

  (** {3 Enable or disable events} *)

  (* TODOC Those explanations are not easy to understand (nor grammatically correct). *)

  (** Disable {! Dom_html.Event} with stopping propagation during capture phase **)
  val disable_event :  'a #Dom.event Js.t Dom_html.Event.typ ->
    #Dom_html.eventTarget Js.t ->
    Dom_html.event_listener_id

  (** Enable {! Dom_html.Event} with id given by disable_event **)
  val enable_event : Dom_html.event_listener_id -> unit

  val enable_events : Dom_html.event_listener_id list -> unit

  val disable_drag_and_drop : #Dom_html.eventTarget Js.t ->
    Dom_html.event_listener_id list


(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 * Vincent Balat
 * Christophe Lecointe
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
  open Eliom_content.Html5
  (** Scrollbar module.

      Allow to add customizable scrollbars to your pages.
      To use it, you must include the original js file, and jQuery to your page.

      Binding of the JS custom scrollbar by Manos Malihutsakis
      @see < http://manos.malihu.gr/jquery-custom-content-scroller/ > Scrollbar of Manos Malihutsakis
      @author Christophe Lecointe
  *)

  (** Describe where the dragger should scroll to.
      [Bottom] and [Top] are for vertical scrollbars, [Left] and [Right] for
      horizontal ones. *)
  (* TODOC : What are First, Last and Int ? *)
  type scroll_t =
    | Bottom
    | First
    | Int of int
    | Last
    | Left
    | Right
    | Top

  (** This function adds a custom scrollbar to the element [elt]. There are
      several optionnal callbacks that you can add at construction, but you can
      also add them later.

      @param height Determine the height of the scrollbar. If none, the
      scrollbar will have the size of the element.

      @param scroll Determine the starting position of the dragger.

      @param inertia Scrolling inertia in milliseconds. Really low
      values (<10) are forced to 10, because it breaks the scrollbar when
      under 10. Low values are irrelevant anyway, since the user can't
      even see it. To disable the inertia, put 0.

      @param mouse_wheel_pixels Mouse wheel scrolling amount in pixel. If
      undefined , the value "auto" is used. *)
  val add_scrollbar :
    ?height:(Dom_html.element Js.t -> int) ->
    ?scroll:scroll_t ->
    ?inertia:int ->
    ?mouse_wheel_pixels:int ->
    ?on_scroll_callback:(unit -> unit) ->
    ?on_scroll_start_callback:(unit -> unit) ->
    ?on_total_scroll_callback:(unit -> unit) ->
    ?on_total_scroll_back_callback:(unit -> unit) ->
    ?while_scrolling_callback:(unit -> unit) ->
    ?on_total_scroll_offset:int ->
    ?on_total_scroll_back_offset:int ->
    'a elt -> unit Lwt.t

  (** Return the position of the dragger. The position is updated only
      when the dragger has finished its movement. Thus, if you call it while the
      dragger is moving, the position returned will be the position of the
      dragger before the scroll *)
  val get_dragger_pos : 'a elt -> int

  (** Return the position of the dragger in the bar in percent.
      As [get_dragger_pos], the position is only updated when the dragger has finished its movement.
  *)
  val get_dragger_pct : 'a elt -> int

  (** Scroll the scrollbar attached to [elt] to a point defined by [scroll].
      It returns a thread which end when the scrolling is done (immediately if inertia is deactivated).*)
  val lwt_scroll_to : ?inertia:bool -> ?scroll:scroll_t  ->
    'a elt -> unit Lwt.t

  (** Update the scrollbar.
      You should call this function each time the content of the element the scrollbar is attached to is changed. *)
  val update : ?height:(Dom_html.element Js.t -> int) ->
    ?scroll:scroll_t -> 'a elt -> unit Lwt.t

  (** Add a function to the list of callbacks executed before a scroll. *)
  val scroll_starts : (unit -> unit) -> 'a elt -> 'a Lwt.t
  (* TODOC : 'a Lwt ? Really ? What is it supposed to be ? *)

  (** Add a function to the list of callbacks executed after a scroll. *)
  val scrolls : (unit -> unit) -> 'a elt -> 'a Lwt.t


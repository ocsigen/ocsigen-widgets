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
  open Eliom_content.Html5
}}

{client{
  open Eliom_content.Html5

  (** Buttons are html elements with a state of [pressed] or [unpressed]
      and actions are generally coupled with a state (such as {b show}/{b hide}
      an element). *)

  (** The type for event triggered by [buttons]. You can add an event listener
      on many events for [buttons], see below. *)
  class type button_event = object
    inherit Dom_html.event
  end

  module Event : sig
    type event = button_event Js.t Dom.Event.typ

    val press : event
    val unpress : event
  end

  (**  {2 Specific events for alerts} *)

  val pre_press
    : ?use_capture:bool -> #Dom_html.eventTarget Js.t -> button_event Js.t Lwt.t
  val pre_unpress
    : ?use_capture:bool -> #Dom_html.eventTarget Js.t -> button_event Js.t Lwt.t

  val press
    : ?use_capture:bool -> #Dom_html.eventTarget Js.t -> button_event Js.t Lwt.t
  val unpress
    : ?use_capture:bool -> #Dom_html.eventTarget Js.t -> button_event Js.t Lwt.t

  val post_press
    : ?use_capture:bool -> #Dom_html.eventTarget Js.t -> button_event Js.t Lwt.t
  val post_unpress
    : ?use_capture:bool -> #Dom_html.eventTarget Js.t -> button_event Js.t Lwt.t

  (** This event is triggered before the [button] is pressed. *)
  val pre_presses :
    ?cancel_handler:bool
    -> ?use_capture:bool
    -> 'a elt
    -> (button_event Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t

  (** This event is triggered when the [button] is pressed. *)
(*VVV EXPLAIN!!! what is the difference with pre_press?
  It seems (but I needed to read the code to understand) that
  press is triggered BEFORE the event, but after pre_press,
  but only if the event is not "prevented", whereas pre_press is
  triggered even if it is prevented.
*)
  val presses :
    ?cancel_handler:bool
    -> ?use_capture:bool
    -> 'a elt
    -> (button_event Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t

  (** This event is triggered after the [button] is pressed. *)
  val post_presses :
    ?cancel_handler:bool
    -> ?use_capture:bool
    -> 'a elt
    -> (button_event Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t

  (** This event is triggered before the [button] is unpressed. *)
  val pre_unpresses :
    ?cancel_handler:bool
    -> ?use_capture:bool
    -> 'a elt
    -> (button_event Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t

  (** This event is triggered when the [button] is unpressed. *)
  val unpresses :
    ?cancel_handler:bool
    -> ?use_capture:bool
    -> 'a elt
    -> (button_event Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t

  (** This event is triggered after the [button] is unpressed. *)
  val post_unpresses :
    ?cancel_handler:bool
    -> ?use_capture:bool
    -> 'a elt
    -> (button_event Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t

  (** {2 Types for buttons} *)

  (** The basic type for [buttons]. You can use it to develop your own kind
      of buttons. It is used with other more specific buttons, such as
      [button_alert] or [button_dyn_alert] (see below).

      Any visible element can be used to simulate a button.

      A button is an item of an [active_set] (see
      {% <<a_api project="ojwidgets" | val Ow_active_set_sigs.T.item>> %}).
      It is only important if the [button] is used with an [active_set], (see
      below).

      A button can be [press], [unpress] or [toggle].

      When a button is [pressed], the css class {b "pressed"} is added to the
      element which refer to the [button]. *)
  class type button = object
    inherit Ow_active_set.item
    inherit Ow_base_widget.widget

    (** Indicates whether the [button] is pressed or not. *)
    method pressed : bool Js.t Js.readonly_prop

    (** Explicitly press the [button]. *)
    method press : unit Js.meth

    (** Explicitly unpress the [button]. *)
    method unpress : unit Js.meth

    (** Presses the [button] if it is unpressed, and unpress the [button] if
        it is pressed. *)
    method toggle : unit Js.meth

    (** {b EXPERIMENTAL}: try to prevents a button from beeing [pressed]. This
        method should be used with [pre_presses] or [pre_unpresses] events. *)
(*VVV What does it mean "shouldbe used with"? *)
    method prevent : bool Js.t -> unit Js.meth
  end

  (** The followings [buttons] use [alerts]. If you're not comfortable with
      [alerts], you should read the doc about them first. *)

  (** This button trigger an alert. When it is [pressed], the alert is shown,
      and when it is [unpressed], the alert is hidden. *)
  class type button_alert = object
    inherit button
  end

  (** Same as above, but use dynamic alert instead. *)
  class type button_dyn_alert = object
    inherit button_alert

    (** Wrapper around [update] method from [dyn_alert] from the
        module [Ow_alert].

        @see 'Ow_alert'.
        *)
    method update : unit Lwt.t Js.meth
  end

  (** {2 Helpers for buttons} *)

  (** Same as [closeable_by_click] from the module [Ow_alert]
      @see 'Ow_alert'. *)
  val closeable_by_click : 'a elt -> 'a elt

  (** {2 Construction functions} *)

  (** Provides behaviours of a basic button.

     [pressed] indicates if the button is initially [pressed] or not.
     The parameter [set] can be used if you want to have only one button
     [pressed] as the same time of other buttons. (as radio buttons).

     The function [predicate] is called before the [button] is pressed.
     If [true] is returned, so the [button] can be press, otherwise, the
     button stay unpressed. *)
  val button :
    ?set:Ow_active_set.t
    -> ?pressed:bool
    -> ?predicate:(unit -> bool Lwt.t)
    -> 'a elt
    -> 'a elt


(*VVV DOC: You must explain what IS an alert button.
  What you are saying is: an alert button is an alert button.
  (same everywhere)
*)
  (** Provides behaviours of an alert button.

     It works as [alert] from [Ow_alert]. The first parameter
     corresponds to the [button] and the second, to the [alert]
     ([button_alert elt elt_alert]).

     For the parameters [set], [predicate] and [pressed], see the description
     above.

     For the parameters [allow_outer_clicks], [before] and [after], see the
     description from the module [Ow_alert]

     @see 'Ow_alert' Ow_alert.

   *)
  val button_alert :
       ?v : Ow_position.v_orientation'
    -> ?h : Ow_position.h_orientation'
    -> ?set:Ow_active_set.t
    -> ?pressed:bool
    -> ?predicate:(unit -> bool Lwt.t)
    -> ?allow_outer_clicks:bool
    -> ?closeable_by_button:bool
    -> ?before:('a elt -> 'b elt -> unit)
    -> ?after:('a elt -> 'b elt-> unit)
    -> 'a elt
    -> 'b elt
    -> ('a elt * 'b elt)

  (** Provides behaviours of an dynamic alert button.

     The parameters are the same as [button_alert]. The third parameter
     correponds to the function used to generate the dynamic content of the
     [alert].

     @see 'Ow_alert' Ow_alert.
   *)
  val button_dyn_alert :
       ?v : Ow_position.v_orientation'
    -> ?h : Ow_position.h_orientation'
    -> ?set:Ow_active_set.t
    -> ?pressed:bool
    -> ?predicate:(unit -> bool Lwt.t)
    -> ?allow_outer_clicks:bool
    -> ?closeable_by_button:bool
    -> ?before:('a elt -> 'b elt -> unit Lwt.t)
    -> ?after:('a elt -> 'b elt-> unit Lwt.t)
    -> 'a elt
    -> 'b elt
    -> ('a elt -> 'b elt -> 'b elt list Lwt.t)
    -> ('a elt * 'b elt)

  (** {2 Conversion functions} *)

  (** These functions check if the given element is an {b instance of} specific
      button ([button], [button_alert] or [button_dyn_alert]). *)

  val to_button : 'a elt -> button Js.t
  val to_button_alert : 'a elt -> button_alert Js.t
  val to_button_dyn_alert : 'a elt -> button_dyn_alert Js.t
}}

{shared{
  type ('a, 'b, 'c) button_dyn_alert_fun' =
    'a elt -> 'b elt -> 'c elt list Lwt.t
}}

{server{
  val closeable_by_click :
     'a elt
  -> 'a elt

  val button :
    ?set:Ow_active_set.t' client_value
  -> ?pressed:bool
  -> ?predicate:(unit -> bool Lwt.t)
  -> 'a elt
  -> 'a elt

  val button_alert :
     ?v : Ow_position.v_orientation'
  -> ?h : Ow_position.h_orientation'
  -> ?set:Ow_active_set.t' client_value
  -> ?pressed:bool
  -> ?predicate:(unit -> bool Lwt.t)
  -> ?allow_outer_clicks:bool
  -> ?closeable_by_button:bool
  -> ?before:('a elt -> 'b elt -> unit)
  -> ?after:('a elt -> 'b elt -> unit)
  -> 'a elt
  -> 'b elt
  -> ('a elt * 'b elt)

  val button_dyn_alert :
     ?v : Ow_position.v_orientation'
  -> ?h : Ow_position.h_orientation'
  -> ?set:Ow_active_set.t' client_value
  -> ?pressed:bool
  -> ?predicate:(unit -> bool Lwt.t)
  -> ?allow_outer_clicks:bool
  -> ?closeable_by_button:bool
  -> ?before:('a elt -> 'b elt -> unit Lwt.t)
  -> ?after:('a elt -> 'b elt -> unit Lwt.t)
  -> 'a elt
  -> 'b elt
  -> ('a, 'b, _) button_dyn_alert_fun' client_value
  -> ('a elt * 'b elt)
}}

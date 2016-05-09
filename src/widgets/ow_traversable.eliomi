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
  open Eliom_content.Html
  open Html_types

[%%client.start]
  open Dom_html
  open Dom


[%%client.start]

  (** A traversable element can iterate through their children. *)
(*VVV This is not a description of the widget. Please fix.
      A "traversable" is much more than a DOM element with an iterator
      (and bwt I don't even see where is the iterator function).
  Is this name used elsewhere for the same concept? Where?
*)

  (**  {2 Specific events for alerts} *)

  (** A type which defines which interaction have been used to trigger
      action on the [traversable] element. *)
  type by = [
      | `Click
      | `Key of int
      | `Explicit
  ]

  (** The [detail] of events of [traversable] element. The method [by]
      indicate how the active element has been set.

      [`Explicit] mean that the function [setActive] from [traversable]
      class has been called to set active an element.

      [`Key] holds the keycode of the last key which set to active an
      element (up/down arrows generally).

      [`Click] means that the active element has been set using mouse.
      *)
  class type traversable_detail_event = object
    method by : by Js.meth
  end

  (** Traversable's event type. *)
  class type traversable_event =
    [traversable_detail_event] Ow_event.customEvent

  module Event : sig
    type event = traversable_event Js.t Dom.Event.typ

    val actives : event
  end

  val active : ?use_capture:bool -> #Dom_html.eventTarget Js.t -> traversable_event Js.t Lwt.t

  (** This event is triggered when an element is to active. Some informations
      are available in the [traversable_event] (see above). *)
  val actives :
    ?cancel_handler:bool
    -> ?use_capture:bool
    -> 'a elt
    -> (traversable_event Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t

  (** {2 Types for traversable} *)

  (** A [traversable] element can use keys to navigate through their
      children (only one level of children).

      The element {b MUST BE} a {b ul} with {b li} children.

      {b li} elements are used as container for child and used as item
      for the [traversable] element. They must respect some requirements.
      They {b MUST HAVE} the css class {b "ojw_traversable_elt"}.

      Traversable is used to compose other widgets (as [Ow_dropdown]).
      *)
  class type traversable = object
    inherit Ow_base_widget.widget

    (** Get the original version of the container used as [traversable]
        element (without conversion). *)
    method getContainer : Html_types.ul elt Js.meth

    (** Explicitly set active the next element of the [traversable]
        element. *)
    method next : unit Js.meth

    (** Explicitly set active the previous element of the [traversable]
        element. *)
    method prev : unit Js.meth

    (** Make inactive the current active element. *)
    method resetActive : unit Js.meth

    (** Explicitly set to active state a [traversable]'s child. *)
    method setActive : Html_types.li elt -> unit Js.meth

    (** Get the current active element. If there is no active element,
        returns [Js.null]. *)
    method getActive : Html_types.li elt Js.opt Js.meth

    (** Returns true if the [traversable] element can listen keys and
        interpret them. Uses the [is_traversable] function on the construction
        of the widget (see below). *)
    method isTraversable : bool Js.meth
  end

  module Style : sig
    val traversable_cls : string
    val traversable_elt_cls : string
    val selected_cls : string
  end

  (** {2 Construction functions} *)

  (** Provides the behaviour of listening keys when the element is traversable.

      Keys up and down arrows are used for navigating through children. You can
      also click an a child to set it to active.

      When an element is active, the css class {b "selected"} is added to the
      {b li} element.

      [enable_link] allows you to keep the default behaviour for the children
      which are links. If it is set to [false], the link won't be interpreted.

      [focus] set the document's focus to the current active element. Beware,
      no checks are done when giving the focus, if you use element without
      focus interactions, you will probably run into a javascript error.

      [is_traversable] indicates whether or not the [traversable] element can
      listen to keys events. If [false] is returned, keys events won't be
      interpreted. This function is called each time an keydown event occurs.

      [on_keydown] is a helper function which is called each time a keydown
      event is triggered. The return value indicates if the event must be
      prevented or not ([Dom.preventDefault] and [Dom_html.stopPropagation]).
      Use this function instead of adding an event listener for keydowns on
      the [traversable] element.

      *)
  val traversable :
     ?enable_link : bool
  -> ?focus : bool
  -> ?is_traversable : (#traversable Js.t -> bool)
  -> ?on_keydown : (Dom_html.keyboardEvent Js.t -> bool Lwt.t)
  -> Html_types.ul elt
  -> Html_types.ul elt

  (** {2 Conversion functions} *)

  (** Check if the given element is an instance of a [traversable] widget. *)
  val to_traversable : Html_types.ul elt -> traversable Js.t


[%%shared.start]
  val li :
    ?a:[< Html_types.li_attrib > `Class `User_data ]
      Eliom_content.Html.D.attrib list
  -> ?anchor:bool
  -> ?href:string
  -> ?value:Html_types.text
  -> ?value_to_match:Html_types.text
  -> Html_types.flow5_without_interactive Eliom_content.Html.D.Raw.elt list
  -> [> Html_types.li ] Eliom_content.Html.D.elt


[%%server.start]
  module Style : sig
    val traversable_cls : string
    val traversable_elt_cls : string
    val selected_cls : string
  end


[%%server.start]
  class type traversable = object end


[%%server.start]
  val traversable :
     ?enable_link:bool
  -> ?focus:bool
    (* TODO
  -> ?is_traversable:(#traversable Js.t -> bool) Eliom_client_value.t
  -> ?on_keydown:(Dom_html.keyboardEvent Js.t -> bool Lwt.t) Eliom_client_value.t
     *)
  -> ul elt
  -> ul elt

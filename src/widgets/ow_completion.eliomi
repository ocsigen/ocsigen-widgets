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
  open Html5_types
}}
{client{
  open Dom_html
  open Dom
}}

{shared{
  type refresh_fun' = int -> string -> li elt list Lwt.t
  type on_confirm_fun' = string -> unit Lwt.t
}}

{client{
  open Eliom_content.Html5

  (** A completion widget to complete on string value. *)

  (** A [completion] widget is [dropdown] widget. The list of the
      possible values are displayed using a [dropdown]. *)
  class type completion = object
    inherit Ow_dropdown.dropdown

    (** You can retrieve the value of the [completion] widget or even
        change it (you need to [refresh] explicitly the widget). *)
    method value : Js.js_string Js.t Js.prop

    (** Clear the list of the possible values. The content will be
        automatically refresh during the next action. *)
    method clear : unit Js.meth

    (** Explicitly confirm with the current value of the input. *)
    method confirm : unit Lwt.t Js.meth

    (** Explicitly refresh the content of the widget (using the given
       function [refresh] on the construction of the widget). *)
    method refresh : unit Lwt.t Js.meth
  end

  (** Provides behaviours of a completion widget.

      The main purpose of this widget is to complete on string value.
      [completion] uses [dropdown] to display matched values. Each item of the
      [dropdown] {b MUST HAVE} an attribute {b data-value}. The value of this
      attribute will be used during comparaison with the input value.

      [refresh limit pattern] must return the list of the different values.
      The [pattern] correspond to the current input value, and [limit] is the
      number of items which will be displayed by the widgets.

      If you don't want to do the comparaison with the value by yourself, you
      can use [auto_match] which will filter the list of elements returned by
      [refresh] function. Element which doesn't match the input value, will be
      ignored and won't be displayed with the [dropdown].

      [accents] indicates if the widget has to take care of accents in the
      {b data-value} attribute and input value.  [sensitive] indicates the case
      has to be insensitive or not.

      If you want to begin the completion from the start of input value, you
      can set [from_start] to [true]. Otherwise, it will try to match the value
      anywhere in the {data-value} string.

      [force_refresh] will automatically force the call to the [refresh]
      function on each actions of the widget. If this option is enabled, the
      rendering could blink.

      [clear_input_on_confirm] will clear the input when method [confirm] is
      called.

      Because [completion] is a [dropdown], and a [dropdown] is composed by
      [traversable] widget, you can navigate through matched values using
      arrow keys. You can also iterate through them using tab key, if the
      option [move_with_tab] is set to [true]

      If [adaptive] is enabled, so the input value will be automatically set
      to the {b data-value} of current active matched element (when navigating
      using arrow keys).

      The function [on_confirm] is called each time the input value is
      confirmed (using [confirm] method or using enter key).

      The widget need an {b input} element as first parameter. The second
      parameter is the container on which the matched values will be
      automatically inserted, it must be a {b ul} element.

      *)
  val completion :
     refresh : refresh_fun'
  -> ?limit : int
  -> ?accents : bool
  -> ?from_start : bool
  -> ?force_refresh : bool
  -> ?sensitive : bool
  -> ?adaptive : bool
  -> ?auto_match : bool
  -> ?clear_input_on_confirm : bool
  -> ?move_with_tab : bool
  -> ?on_confirm : on_confirm_fun'
  -> 'a elt
  -> Html5_types.ul elt
  -> ('a elt * Html5_types.ul elt)
}}

{server{
  (* w1 is a completion of w0. ex: is_completed_by "e" "eddy" = yes *)
  (* both arg are utf8 caml string *)
  val is_completed_by : string -> string -> bool
}}

{shared{
  val li :
    ?a:[< Html5_types.li_attrib > `Class `User_data ]
      Eliom_content.Html5.D.attrib list
  -> value:Html5_types.text
  -> value_to_match:Html5_types.text
  -> Html5_types.flow5_without_interactive Eliom_content.Html5.D.Raw.elt list
  -> [> Html5_types.li ] Eliom_content.Html5.D.elt
}}

{server{
  val completion :
     refresh:refresh_fun' client_value
  -> ?limit:int
  -> ?accents:bool
  -> ?sensitive:bool
  -> ?adaptive:bool
  -> ?auto_match:bool
  -> ?clear_input_on_confirm:bool
  -> ?move_with_tab:bool
  -> ?on_confirm:on_confirm_fun' client_value
  -> 'a elt
  -> ul elt
  -> ('a elt * ul elt)
}}

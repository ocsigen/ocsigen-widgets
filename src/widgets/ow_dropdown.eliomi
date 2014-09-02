{shared{
  open Eliom_content.Html5
  open Html5_types
  open Ow_dom
}}
{client{
  open Dom_html
  open Dom
}}

{client{
  (** A dropdown menu is a menu which can be displayed under an element
      which will act like a [button]. *)

  (**  {2 Specific events for dropdowns} *)

  (** @see 'Ow_button'. *)
  (** @see 'Ow_traversable'. *)

  (** {2 Types for dropdown} *)

  (** A [dropdown] is a kind of [button_alert] which triggers the menu when clicking
      on it (you can also use it with hover events, see below). It also uses
      a [traversable] element to simulate the menu (and the interactions with
      the keys).
      *)
  class type dropdown = object
    inherit Ow_button.button_alert

    (** Returns the [traversable] element of the [dropdown]. *)
    method traversable : Ow_traversable.traversable Js.t Js.readonly_prop
  end

  (** {2 Construction functions} *)

  (** Provides behaviours of dropdown menu.

      Some of the parameters are the same as [Ow_button] and [Ow_traversable].

      The parameters [v] and [h] (respectively vertical and horizontal)
      corresponds to the orientation of the menu.

      You can use [hover] and [hover_timeout] if you want your [dropdown]
      triggered during hover javascript events. The [dropdown] waits
      [hover_timeout] seconds before hiding the menu.

      The [dropdown] is traversable only when it is opened.

      @see 'Ow_button'.
      @see 'Ow_traversable'.
      @see 'Ow_position'.
      *)
  val dropdown :
     ?v : Ow_position.v_orientation'
  -> ?h : Ow_position.h_orientation'
  -> ?focus : bool
  -> ?hover : bool
  -> ?hover_timeout : float
  -> ?enable_link : bool
  -> ?is_traversable : (#dropdown Js.t -> bool)
  -> ?predicate : (unit -> bool Lwt.t)
  -> ?on_keydown : (Dom_html.keyboardEvent Js.t -> bool Lwt.t)
  -> 'a elt
  -> Html5_types.ul elt
  -> ('a elt * Html5_types.ul elt)

  (* FIXME: add conversion functions. *)
}}

{shared{
  val li :
    ?a:[< Html5_types.li_attrib > `Class `User_data ]
      Eliom_content.Html5.D.attrib list
  -> href:string
  -> Html5_types.flow5_without_interactive Eliom_content.Html5.D.Raw.elt list
  -> [> Html5_types.li ] Eliom_content.Html5.D.elt
}}

{server{
  val dropdown :
     ?v : Ow_position.v_orientation'
  -> ?h : Ow_position.h_orientation'
  -> ?hover:bool
  -> ?hover_timeout:float
  -> 'a elt
  -> ul elt
  -> ('a elt * ul elt)
}}

{client{
  open Eliom_content.Html5

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
}}

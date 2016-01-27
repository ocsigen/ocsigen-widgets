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

[%%shared
  open Eliom_content.Html5
  open Html5_types
]

[%%client
  open Dom
  open Dom_html

  class type alert_event = object
    inherit Dom_html.event
  end

  module Event = struct
    type event = alert_event Js.t Dom.Event.typ

    module S = struct
      let show = "show"
      let hide = "hide"
      let outer_click = "outer_click"
    end

    let show : event = Dom.Event.make S.show
    let hide : event = Dom.Event.make S.hide
    let outer_click : event = Dom.Event.make S.outer_click
  end

  let show ?use_capture target =
    Lwt_js_events.make_event Event.show ?use_capture target
  let hide ?use_capture target =
    Lwt_js_events.make_event Event.hide ?use_capture target
  let outer_click ?use_capture target =
    Lwt_js_events.make_event Event.outer_click ?use_capture target


  let shows ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop show ?cancel_handler ?use_capture (To_dom.of_element t)
  let hides ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop hide ?cancel_handler ?use_capture (To_dom.of_element t)
  let outer_clicks ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop outer_click ?cancel_handler ?use_capture (To_dom.of_element t)

  class type alert = object
    inherit Ow_base_widget.widget

    method visible : bool Js.meth
    method show : unit Js.meth
    method hide : unit Js.meth
  end

  class type alert' = object
    inherit alert

    method _visible : (#alert Js.t, bool) Js.meth_callback Js.prop
    method _show : (#alert Js.t, unit) Js.meth_callback Js.prop
    method _hide : (#alert Js.t, unit) Js.meth_callback Js.prop
  end

  class type dyn_alert = object
    inherit Ow_base_widget.widget

    method visible : bool Js.meth
    method show : unit Lwt.t Js.meth
    method hide : unit Js.meth
    method update : unit Lwt.t Js.meth
  end

  class type dyn_alert' = object
    inherit dyn_alert

    method _visible : (#dyn_alert Js.t, bool) Js.meth_callback Js.prop
    method _show : (#dyn_alert Js.t, unit Lwt.t) Js.meth_callback Js.prop
    method _hide : (#dyn_alert Js.t, unit) Js.meth_callback Js.prop
    method _update : (#dyn_alert Js.t, unit Lwt.t) Js.meth_callback Js.prop
  end

  module Style = struct
    let alert_cls = "ojw_alert"
    let dyn_alert_cls = "ojw_dyn_alert"
  end

  exception Close_button_not_in_alert

  let closeable_by_click elt =
    let unsafe_elt elt = (Js.Unsafe.coerce elt :> Dom_html.element Js.t) in
    let get_parent close =
      let rec aux node =
        Js.Opt.case (node##.parentNode)
          (fun () -> raise Close_button_not_in_alert)
          (fun p ->
             let p' = unsafe_elt p in
             if Js.to_bool (p'##.classList##(contains (Js.string Style.alert_cls)))
             || Js.to_bool (p'##.classList##(contains (Js.string Style.dyn_alert_cls)))
             then p
             else aux p')
      in aux (unsafe_elt close)
    in
    let on_close elt = ((Js.Unsafe.coerce elt))##hide in
    Ow_tools.closeable_by_click
      ~get_parent ~on_close (To_dom.of_element elt);
    elt

  let created_alerts = ref ([] : alert Js.t list)

  let get_display elt' =
    let elt' = Ow_fun.getComputedStyle elt' in
    Js.string (match (Js.to_string elt'##.display) with
        | "none" -> "block" (* should we force ? *)
        | "" -> "block" (* should we force ? *)
        | display -> display
      )

  let prevent_outer_clicks elt =
    (*
    (Js.Unsafe.coerce elt')##preventOuterClick <- Js._true
    *)
    Lwt.async (fun () ->
        (* FIXME: use another module ? Which corresponds to any dom element ? *)
      Lwt_js_events.clicks (To_dom.of_element elt)
        (fun e _ ->
          Dom_html.stopPropagation e;
          Lwt.return ()))

  let to_alert elt = (Js.Unsafe.coerce (To_dom.of_element elt) :> alert Js.t)
  let to_dyn_alert elt = (Js.Unsafe.coerce (To_dom.of_element elt) :> dyn_alert Js.t)

  let alert
      ?(show = false)
      ?(allow_outer_clicks = false)
      ?(on_outer_click = (fun elt' -> elt'##hide))
      ?(before = (fun _ -> ()))
      ?(after = (fun _ -> ()))
      elt =
    let elt' = (Js.Unsafe.coerce (To_dom.of_element elt) :> alert' Js.t) in
    let meth = Js.wrap_meth_callback in

    elt'##.classList##(add (Js.string Style.alert_cls));

    if not allow_outer_clicks then begin
      created_alerts := (elt' :> alert Js.t)::!created_alerts;
      prevent_outer_clicks elt;
    end;

    let display = get_display elt' in

    elt'##._show :=
    meth (fun this ->
      if not this##visible then begin
        (* Could blink, FIXME: should be set after the [before] function. *)
        this##.style##.display := display;
        before elt;
        Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.show);
        after elt;
      end;
    );

    elt'##._hide :=
    meth (fun this ->
      Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.hide);
      this##.style##.display := Js.string "none";
      ()
    );

    elt'##._visible :=
    meth (fun this ->
      not (this##.style##.display = (Js.string "none"))
    );

    if show then
      elt'##show
    else
      elt'##hide;

    Lwt.async (fun () ->
      outer_clicks elt
        (fun _ _ ->
           on_outer_click (elt' :> alert Js.t);
           Lwt.return ()));

    elt

  (** Re-write it in a more DRY way. *)
  let dyn_alert
      ?(show = false)
      ?(allow_outer_clicks = false)
      ?(on_outer_click = (fun elt' -> elt'##hide))
      ?(before = (fun _ -> Lwt.return ()))
      ?(after = (fun _ -> Lwt.return ()))
      elt f =
    let elt' = (Js.Unsafe.coerce (To_dom.of_element elt) :> dyn_alert' Js.t) in
    let meth = Js.wrap_meth_callback in

    ignore (alert ~allow_outer_clicks elt);

    elt'##.classList##(add (Js.string Style.dyn_alert_cls));

    let display = get_display elt' in

    let internal_show ?(event = true) ?(update_display = true) this =
      (* Could blink, FIXME: should be set after the [before] function. *)
      if update_display then
        this##.style##.display := display;
      let%lwt () = before elt in
      let%lwt cnt = f elt in
      List.iter
        (fun c -> appendChild elt' (To_dom.of_element c))
        cnt;
      if event then
        Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.show);
      let%lwt () = after elt in
      Lwt.return ()
    in

    let internal_clear () =
      List.iter
        (removeChild elt')
        (list_of_nodeList elt'##.childNodes)
    in

    elt'##._show :=
    meth (fun this ->
      if not this##visible then begin
        internal_show this
      end else Lwt.return ()
    );

    elt'##._hide :=
    meth (fun this ->
      Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.hide);
      this##.style##.display := Js.string "none";
      internal_clear ()
    );

    elt'##._update :=
    meth (fun this ->
      internal_clear ();
      internal_show ~event:false ~update_display:false this;
    );

    if show then
      Lwt.async (fun () -> elt'##show)
    else
      elt'##hide;

    Lwt.async (fun () ->
      outer_clicks elt
        (fun _ _ ->
           on_outer_click (elt' :> dyn_alert Js.t);
           Lwt.return ()));

    elt

  let () =
    Lwt.async (fun () ->
      Lwt_js_events.clicks document
        (fun e _ ->
           let close_opened_alerts () =
             List.iter
               (fun elt' ->
                 if elt'##visible then
                   Ow_event.dispatchEvent elt'
                     (Ow_event.customEvent Event.S.outer_click))
               !created_alerts
           in
           close_opened_alerts ();
           (*
           (Js.Optdef.iter (e##toElement)
              (fun elt' ->
                 Js.Opt.iter (elt')
                   (fun elt' ->
                      Js.Optdef.case ((Js.Unsafe.coerce elt')##preventOuterClick)
                        (close_opened_alerts)
                        (fun prevent ->
                           if prevent = Js._false then
                             close_opened_alerts ()))));
                             *)
            Lwt.return ()))

]

[%%shared
  type ('a, 'b) dyn_alert_fun' = 'a elt -> 'b elt list Lwt.t
]

[%%server
  let closeable_by_click (elt : 'a elt) =
    ignore [%client ( ignore (closeable_by_click ~%elt) : unit)];
    elt

  let alert
      ?(allow_outer_clicks : bool option)
      ?(before : ('a elt -> unit) option)
      ?(after : ('a elt -> unit) option)
      (elt : 'a elt) =
    ignore [%client (
        ignore (
          alert
            ?allow_outer_clicks:~%allow_outer_clicks
            ?before:~%before
            ?after:~%after
            ~%elt
        )
    : unit)];
    elt

  let dyn_alert
      ?(allow_outer_clicks : bool option)
      ?(before : ('a elt -> unit Lwt.t) option)
      ?(after : ('a elt -> unit Lwt.t) option)
      (elt : 'a elt)
      (f : ('a, _) dyn_alert_fun' client_value) =
    ignore [%client (
        ignore (
          dyn_alert
            ?allow_outer_clicks:~%allow_outer_clicks
            ?before:~%before
            ?after:~%after
            ~%elt
            ~%f
        )
    : unit)];
    elt
]

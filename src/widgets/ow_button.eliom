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
]

[%%client
  class type button = object
    inherit Ow_active_set.item
    inherit Ow_base_widget.widget

    method pressed : bool Js.t Js.readonly_prop

    method prevent : bool Js.t -> unit Js.meth
    method press : unit Js.meth
    method unpress : unit Js.meth
    method toggle : unit Js.meth
  end

  class type button' = object
    inherit button

    inherit Ow_active_set.item'
    inherit Ow_base_widget.widget'

    method _prevented : bool Js.t Js.prop
    method _prevent : (#button Js.t, bool Js.t -> unit) Js.meth_callback Js.prop

    method _press : (#button Js.t, unit) Js.meth_callback Js.prop
    method _unpress : (#button Js.t, unit) Js.meth_callback Js.prop
    method _toggle : (#button Js.t, unit) Js.meth_callback Js.prop
  end

  class type button_alert = object
    inherit button
  end

  class type button_dyn_alert = object
    inherit button_alert

    method update : unit Lwt.t Js.meth
  end

  class type button_dyn_alert' = object
    inherit button_dyn_alert

    method _update : (#button_dyn_alert Js.t, unit Lwt.t) Js.meth_callback Js.prop
  end

  class type button_event = object
    inherit Dom_html.event
  end

  module Event = struct
    type event = button_event Js.t Dom.Event.typ

    module S = struct
      let press = "press"
      let unpress = "unpress"

      let pre_press = "pre_press"
      let pre_unpress = "pre_unpress"

      let post_press = "post_press"
      let post_unpress = "post_unpress"
    end

    let press : event = Dom.Event.make S.press
    let unpress : event = Dom.Event.make S.unpress

    let pre_press : event = Dom.Event.make S.pre_press
    let pre_unpress : event = Dom.Event.make S.pre_unpress

    let post_press : event = Dom.Event.make S.post_press
    let post_unpress : event = Dom.Event.make S.post_unpress
  end

  let press ?use_capture target =
    Lwt_js_events.make_event Event.press ?use_capture target
  let unpress ?use_capture target =
    Lwt_js_events.make_event Event.unpress ?use_capture target

  let pre_press ?use_capture target =
    Lwt_js_events.make_event Event.pre_press ?use_capture target
  let pre_unpress ?use_capture target =
    Lwt_js_events.make_event Event.pre_unpress ?use_capture target

  let post_press ?use_capture target =
    Lwt_js_events.make_event Event.post_press ?use_capture target
  let post_unpress ?use_capture target =
    Lwt_js_events.make_event Event.post_unpress ?use_capture target


  let presses ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop
      press ?cancel_handler ?use_capture (To_dom.of_element t)
  let unpresses ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop
      unpress ?cancel_handler ?use_capture (To_dom.of_element t)

  let pre_presses ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop
      pre_press ?cancel_handler ?use_capture (To_dom.of_element t)
  let pre_unpresses ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop
      pre_unpress ?cancel_handler ?use_capture (To_dom.of_element t)

  let post_presses ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop
      post_press ?cancel_handler ?use_capture (To_dom.of_element t)
  let post_unpresses ?cancel_handler ?use_capture t =
    Lwt_js_events.seq_loop
      post_unpress ?cancel_handler ?use_capture (To_dom.of_element t)

  let default_predicate () =
    Lwt.return true

  let button ?set ?(pressed = false) ?(predicate = default_predicate) elt =
    let elt' = (Js.Unsafe.coerce (To_dom.of_element elt) : button' Js.t) in
    let meth = Js.wrap_meth_callback in

    let wbutton b = (Js.Unsafe.coerce b : button' Js.t) in

    let internal_press b =
      let this = (elt' :> button Js.t) in
      (Js.Unsafe.coerce this)##.pressed := Js.bool b;
      if Js.to_bool this##.pressed
      then this##.classList##(add (Js.string "pressed"))
      else this##.classList##(remove (Js.string "pressed"))
    in

    ignore (Ow_base_widget.ctor elt' "button");
    ignore (
      Ow_active_set.ctor
        ~enable:(fun this ->
            let wthis = wbutton this in
            Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.pre_press);
            if not (wthis##._prevented = Js._true) then begin
              Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.press);
              internal_press true;
              Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.post_press);
            end;
            wthis##._prevented := Js._false
          )
        ~disable:(fun this ->
            let wthis = wbutton this in
            Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.pre_unpress);
            if not ((wbutton this)##._prevented = Js._true) then begin
              Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.unpress);
              internal_press false;
              Ow_event.dispatchEvent this (Ow_event.customEvent Event.S.post_unpress);
            end;
            wthis##._prevented := Js._false
          )
        elt'
    );

    elt'##._press :=
    meth (fun this ->
      match set with
      | None -> this##enable
      | Some set ->
          Ow_active_set.enable ~set (this :> Ow_active_set.item Js.t)
    );

    elt'##._unpress :=
    meth (fun this ->
      match set with
      | None -> this##disable
      | Some set ->
          Ow_active_set.disable ~set (this :> Ow_active_set.item Js.t)
    );

    elt'##._toggle :=
    meth (fun this ->
      if Js.to_bool this##.pressed
      then this##unpress
      else this##press
    );

    elt'##._prevented := Js._false;

    elt'##._prevent :=
    meth (fun this prevent ->
      (wbutton this)##._prevented := prevent;
    );

    (Js.Unsafe.coerce elt')##.pressed := false;
    if pressed then
      elt'##press
    else
      elt'##unpress;

    Lwt.async (fun () ->
      Lwt_js_events.clicks (To_dom.of_element elt)
        (fun e _ ->
           let%lwt ret = predicate () in
           if ret then
             elt'##toggle;
           Lwt.return ()));
    elt

  let default_v = `bottom
  let default_h = `center

  let button_alert
      ?v ?h
      ?set ?pressed
      ?predicate
      ?allow_outer_clicks
      ?(closeable_by_button = true)
      ?before
      ?after
      elt elt_alert =
    let elt' = (Js.Unsafe.coerce (To_dom.of_element elt) : button_alert Js.t) in
    let elt_alert' = Ow_alert.to_alert elt_alert in

    let before, after =
      match before, after with
      | None, None -> (fun _ -> ()), (fun _ -> ())
      | Some before, None -> before elt, (fun _ -> ())
      | None, Some after  -> (fun _ -> ()), after elt
      | Some before, Some after  -> before elt, after elt
    in

    let on_outer_click _ =
      elt'##unpress
    in

    ignore (Ow_alert.alert
              ?allow_outer_clicks ~on_outer_click ~before ~after elt_alert);
    Ow_alert.prevent_outer_clicks elt;

    let position_to v h =
      Ow_position.relative_move ~v ~h
        ~relative:(To_dom.of_element elt)
        elt_alert'
    in
    begin match v, h with
      | Some v, None -> position_to v default_h
      | None, Some h -> position_to default_v h
      | Some v, Some h -> position_to v h
      | None, None -> ()
    end;

    Lwt.async (fun () ->
      pre_unpresses elt
        (fun _ _ ->
           elt'##(prevent (Js.bool (not closeable_by_button)));
           Lwt.return ()));

    Lwt.async (fun () ->
      presses elt
        (fun _ _ ->
           Ow_log.log "show";
           elt_alert'##show;
           Lwt.return ()));

    Lwt.async (fun () ->
      unpresses elt
        (fun _ _ ->
           elt_alert'##hide;
           Lwt.return ()));

    Lwt.async (fun () ->
      Ow_alert.outer_clicks elt
        (fun _ _ ->
           elt'##unpress;
           Lwt.return ()));

    (* We want to listen events before unpress or press the button *)
    ignore (button ?set ?pressed ?predicate elt);

    (elt, elt_alert)

    let button_dyn_alert
      ?v ?h
      ?set ?pressed
      ?predicate
      ?allow_outer_clicks
      ?(closeable_by_button = true)
      ?before
      ?after
      elt elt_alert f =
    let elt' =
      (Js.Unsafe.coerce (To_dom.of_element elt) : button_dyn_alert' Js.t) in
    let elt_alert' = Ow_alert.to_dyn_alert elt_alert in
    let meth = Js.wrap_meth_callback in

    let before, after =
      match before, after with
      | None, None -> (fun _ -> Lwt.return ()), (fun _ -> Lwt.return ())
      | Some before, None -> before elt, (fun _ -> Lwt.return ())
      | None, Some after  -> (fun _ -> Lwt.return ()), after elt
      | Some before, Some after  -> before elt, after elt
    in

    let on_outer_click _ =
      elt'##unpress
    in

    ignore (Ow_alert.dyn_alert
              ?allow_outer_clicks ~on_outer_click
              ~before ~after elt_alert (f elt));
    Ow_alert.prevent_outer_clicks elt;

    let position_to v h =
      Ow_position.relative_move ~v ~h:default_h
        ~relative:(To_dom.of_element elt)
        elt_alert'
    in
    begin match v, h with
      | Some v, None -> position_to v default_h
      | None, Some h -> position_to default_v h
      | Some v, Some h -> position_to v h
      | None, None -> ()
    end;

    elt'##._update :=
    meth (fun this ->
      elt_alert'##update;
    );

    Lwt.async (fun () ->
      pre_unpresses elt
        (fun _ _ ->
           elt'##(prevent (Js.bool (not closeable_by_button)));
           Lwt.return ()));

    Lwt.async (fun () ->
      presses elt
        (fun _ _ ->
           let%lwt () = elt_alert'##show in
           Lwt.return ()));

    Lwt.async (fun () ->
      unpresses elt
        (fun _ _ ->
           elt_alert'##hide;
           Lwt.return ()));

    (* We want to listen events before unpress or press the button *)
    ignore (button ?set ?pressed ?predicate elt);

    (elt, elt_alert)

  let closeable_by_click = Ow_alert.closeable_by_click

  let to_button elt = (Js.Unsafe.coerce (To_dom.of_element elt) :> button Js.t)
  let to_button_alert elt = (Js.Unsafe.coerce (To_dom.of_element elt) :> button_alert Js.t)
  let to_button_dyn_alert elt = (Js.Unsafe.coerce (To_dom.of_element elt) :> button_dyn_alert Js.t)
]

[%%shared
  type ('a, 'b, 'c) button_dyn_alert_fun' =
    'a elt -> 'b elt -> 'c elt list Lwt.t
]

[%%server
  let closeable_by_click = Ow_alert.closeable_by_click

  let button
      ?(set : Ow_active_set.t' Eliom_client_common.client_value option)
      ?(pressed : bool option)
      ?(predicate : (unit -> bool Lwt.t) option)
      (elt : 'a elt) =
    ignore [%client (
        ignore (
          let button = match ~%set with
            | None -> button ?set:None
            | Some set -> button ~set:(Ow_active_set.of_server_set set)
          in
          button
            ?pressed:~%pressed
            ?predicate:~%predicate
            ~%elt
        )
     : unit)];
    elt

  let button_alert
        ?(v : Ow_position.v_orientation' option)
        ?(h : Ow_position.h_orientation' option)
        ?(set : Ow_active_set.t' Eliom_client_common.client_value option)
        ?(pressed : bool option)
        ?(predicate : (unit -> bool Lwt.t) option)
        ?(allow_outer_clicks : bool option)
        ?(closeable_by_button : bool option)
        ?(before : ('a elt -> 'b elt -> unit) option)
        ?(after : ('a elt -> 'b elt -> unit) option)
        (elt : 'a elt)
        (elt_alert : 'b elt) =
    ignore [%client (
        ignore (
          let button_alert = match ~%set with
            | None -> button_alert ?set:None
            | Some set -> button_alert ~set:((Ow_active_set.of_server_set set)
                                             :> Ow_active_set.t)
          in
          button_alert
            ?v:~%v
            ?h:~%h
            ?pressed:~%pressed
            ?predicate:~%predicate
            ?allow_outer_clicks:~%allow_outer_clicks
            ?closeable_by_button:~%closeable_by_button
            ?before:~%before
            ?after:~%after
            ~%elt
            ~%elt_alert
        )
    : unit)];
    (elt, elt_alert)

  let button_dyn_alert
        ?(v : Ow_position.v_orientation' option)
        ?(h : Ow_position.h_orientation' option)
        ?(set : Ow_active_set.t' Eliom_client_common.client_value option)
        ?(pressed : bool option)
        ?(predicate : (unit -> bool Lwt.t) option)
        ?(allow_outer_clicks : bool option)
        ?(closeable_by_button : bool option)
        ?(before : ('a elt -> 'b elt -> unit Lwt.t) option)
        ?(after : ('a elt -> 'b elt -> unit Lwt.t) option)
        (elt : 'a elt)
        (elt_alert : 'b elt)
        (f : ('a, _, _) button_dyn_alert_fun' Eliom_client_common.client_value) =
    ignore [%client (
        ignore (
          let alert = match ~%set with
            | None -> button_dyn_alert ?set:None
            | Some set ->
                button_dyn_alert ~set:((Ow_active_set.of_server_set set) :> Ow_active_set.t)
          in
          button_dyn_alert
            ?v:~%v
            ?h:~%h
            ?pressed:~%pressed
            ?predicate:~%predicate
            ?allow_outer_clicks:~%allow_outer_clicks
            ?closeable_by_button:~%closeable_by_button
            ?before:~%before
            ?after:~%after
            ~%elt
            ~%elt_alert
            ~%f
        )
    : unit)];
    (elt, elt_alert)
]

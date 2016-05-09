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
  open Eliom_content.Html
  open Html_types
]
[%%client
  open Dom_html
  open Dom
]

[%%client
  open Dom
  open Dom_html
  open Eliom_content.Html

  let global_bg = ref (None : divElement Js.t option)

  let get_global_bg () =
    let update_bg bg =
      let w, h =
        let w, h = Ot_size.get_screen_size () in
        Js.Optdef.case (window##.innerWidth)
          (fun () -> w)
          (fun w -> w),
        Js.Optdef.case (window##.innerHeight)
          (fun () -> h)
          (fun h -> h)
      in
      bg##.style##.height := Ot_size.pxstring_of_int h;
      bg##.style##.width := Ot_size.pxstring_of_int w;
    in
    match !global_bg with
      | Some bg ->
          update_bg bg;
          bg
      | None ->
          let bg = createDiv Dom_html.document in
          bg##.classList##(add (Js.string "ojw_background"));
          global_bg := Some bg;
          update_bg bg;
          appendChild document##.body bg;
          bg

  module Style = struct
    let popup_cls = "ojw_popup"
  end

  exception Close_button_not_in_popup

  let show_background () =
    (get_global_bg ())##.style##.visibility := Js.string "visible"

  let hide_background () =
    (get_global_bg ())##.style##.visibility := Js.string "hidden"

  let define_popup ~bg ?(with_background = true) elt =
    (To_dom.of_element elt)##.classList##(add (Js.string Style.popup_cls));

    Lwt.async (fun () ->
      Ow_alert.shows elt
        (fun _ _ ->
           if with_background then
             show_background ();
           Lwt.return ()));

    Lwt.async (fun () ->
      Ow_alert.hides elt
        (fun _ _ ->
           if with_background then (
             Ow_log.log "with bg";
             hide_background ();
           );
           Lwt.return ()))

  let popup ?show ?allow_outer_clicks ?with_background elt =
    let bg = get_global_bg () in
    let before elt =
      Ow_position.absolute_move
        ~h:`center ~v:`center ~scroll:false ~position:`fixed
        ~relative:bg (To_dom.of_element elt);
    in

    define_popup ?with_background ~bg elt;
    ignore (Ow_alert.alert ~before ?show ?allow_outer_clicks elt);

    elt

  let dyn_popup ?show ?allow_outer_clicks ?with_background elt f =
    let bg = get_global_bg () in
    let before elt =
      Ow_position.absolute_move
        ~h:`center ~v:`center ~scroll:false ~position:`fixed
        ~relative:bg (To_dom.of_element elt);
      Lwt.return ()
    in

    define_popup ?with_background ~bg elt;
    ignore (Ow_alert.dyn_alert ~before ?show ?allow_outer_clicks elt f);

    elt

  let to_popup = Ow_alert.to_alert
  let to_dyn_popup = Ow_alert.to_dyn_alert
]

[%%shared
  let closeable_by_click = Ow_alert.closeable_by_click
]

[%%server

  let popup
      ?(show : bool option)
      ?(allow_outer_clicks : bool option)
      ?(with_background : bool option)
      (elt : 'a elt) =
    ignore [%client (
        ignore (
          popup
            ?show:~%show
            ?allow_outer_clicks:~%allow_outer_clicks
            ?with_background:~%with_background
            ~%elt
        )
      : unit)];
    elt

  let dyn_popup
      ?(show : bool option)
      ?(allow_outer_clicks : bool option)
      ?(with_background : bool option)
      (elt : 'a elt)
      (f : ('a, _) Ow_alert.dyn_alert_fun' Eliom_client_value.t) =
    ignore [%client (
        ignore (
          dyn_popup
            ?show:~%show
            ?allow_outer_clicks:~%allow_outer_clicks
            ?with_background:~%with_background
            ~%elt
            ~%f
        )
    : unit)];
    elt
]

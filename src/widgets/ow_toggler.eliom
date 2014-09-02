(* Eliom-base-app
 * http://www.ocsigen.org/eliom-base-app
 *
 * Copyright (C) 2014
 *      Vincent Balat
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

(** TEMPORARY MODULE
    that is here only because I can find some features any more in
    eliom-widgets.
    (positioned_alert for example.
    dyn_alert do not work, because the container is emptied every time
    the alert is shown!!
    FIX in EW!
*)

{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F

type radio_set = (unit -> unit Lwt.t) ref

}}


{client{
let new_radio_set () : radio_set = ref Lwt.return

(** Check whether an element has a class *)
let has_class elt cl =
  Js.to_bool (elt##classList##contains(Js.string cl))

(** Adding a class to an element *)
let add_class str elt = elt##classList##add(Js.string str)

(** Removing a class to an element *)
let remove_class str elt = elt##classList##remove(Js.string str)




(** Something that behave like a button, with press, unpress and switch action.
    If [pressed] is true (default false) the button is pressed by default.
    If [button] is present (with some DOM element), this element will be used
    as a button: click on it will trigger actions open or close alternatively.
    If [set] is present, the button will act like a radio button: only one
    with the same set can be opened at the same time.
    Call function [new_radio_set] to create a new set.
    If [button_closeable] is false, then the button will open but not close.
    If [method_closeable] is false, then the unpress method will have no effect.
    If both are false, the only way to unpress is
    to press another one belonging to the same set.

    Redefine [press_action] and [unpress_action] for your needs.

    Redefine [pre_press] [post_press] [pre_unpress] or [post_unpress]
    if you want something to happen just before or after pressing/unpressing.
*)
class toggler ?(pressed = false) ?button ?set
  ?(method_closeable=true) ?(button_closeable=true) () =
  let set_close_last, close_last = match set with
    | None -> (fun _ -> ()), Lwt.return
    | Some r -> (fun f -> r := f), (fun () -> !r ())
  in
object (me)
  val mutable press_state = pressed
  method pre_press = Lwt.return ()
  method post_press = Lwt.return ()
  method pre_unpress = Lwt.return ()
  method post_unpress = Lwt.return ()
  method press_action = Lwt.return ()
  method unpress_action = Lwt.return ()
  method pressed = press_state
  method press =
    lwt () = close_last () in
    set_close_last (fun () -> me#really_unpress);
    press_state <- true;
    Eliom_lib.Option.iter (add_class "ew_pressed") button;
    lwt () = me#pre_press in
    lwt () = me#press_action in
    me#post_press
  method private really_unpress =
    set_close_last Lwt.return;
    press_state <- false;
    Eliom_lib.Option.iter (remove_class "ew_pressed") button;
    lwt () = me#pre_unpress in
    lwt () = me#unpress_action in
    me#post_unpress
  method unpress = if method_closeable then me#really_unpress else Lwt.return ()
  method switch = if press_state then me#unpress else me#press
  method private button_switch = if press_state
    then (if button_closeable then me#really_unpress else Lwt.return ())
    else me#press
  initializer
    if pressed
    then begin
      set_close_last (fun () -> me#really_unpress);
      Eliom_lib.Option.iter (add_class "ew_pressed") button
    end;
    match button with
      | None -> ()
      | Some b -> Lwt_js_events.async
        (fun () -> Lwt_js_events.clicks b (fun _ _ -> me#button_switch))
end

(** Alert displays an alert box when a button is pressed.
    [get_node] returns the list of elements to be displayed and

    Redefine [get_node] for your needs.

    If you want the alert to be opened at start,
    give an element as [pressed] parameter.
    It must have at the right parent in the page (body by default).

    After getting the node,
    the object is inserted as JS field [o] of the DOM element of
    the alert box.
*)
class [ 'a ] alert ?pressed ?button ?set ?method_closeable ?button_closeable
  ?(parent_node = (Dom_html.document##body :> Dom_html.element Js.t))
  ?(class_=[]) () =
object (me)
  inherit toggler ~pressed:(pressed <> None) ?button ?set
    ?method_closeable ?button_closeable ()
  val mutable node = None
  method get_node : 'a Eliom_content.Html5.D.elt list Lwt.t
    = Lwt.return []
  method press_action =
    lwt n = me#get_node in
    let n = D.div ~a:[a_class ("ew_alert"::class_)] n in
    (Js.Unsafe.coerce (To_dom.of_div n))##o <- me;
    let n = To_dom.of_div n in
    node <- Some n;
    Dom.appendChild parent_node n;
    Lwt.return ()
  method unpress_action =
    (match node with
      | None -> ()
      | Some n -> node <- None; try Dom.removeChild parent_node n with _ -> ());
    Lwt.return ()
  initializer
    match pressed with
      | None -> ()
      | Some elt -> node <- pressed
end

(** show_hide shows or hides a box when pressed/unpressed.
    Set style property "display: none" for unpressed elements if
    you do not want them to appear shortly when the page is displayed.
*)
class [ 'a ] show_hide ?(pressed = false) ?button ?set
  ?method_closeable ?button_closeable elt =
object (me)
  inherit toggler ~pressed ?button ?set ?method_closeable ?button_closeable ()
  method press_action =
    elt##style##display <- Js.string "block"; (*VVV No: restore default value *)
    Lwt.return ()
  method unpress_action =
    elt##style##display <- Js.string "none";
    Lwt.return ()
  initializer
    if not pressed
    then elt##style##display <- Js.string "none" (*VVV will blink ... *)
end



class [ 'a ] positioned_alert
    ?pressed
    ?button
    ?set
    ?method_closeable
    ?button_closeable
    ?parent_node
    ?(class_ = [])
    ?(orientation = `Bottom)
    ~(attach_to : 'a Eliom_content.Html5.D.elt) (* always same as button? *)
    ()
  =

  object(me)

    val mutable att' = To_dom.of_element attach_to

    inherit [ 'a ] alert
        ?pressed
        ?button
        ?set
        ?method_closeable
        ?button_closeable
        ?parent_node
        ~class_:("oui_positioned"::class_)
        ()
      as papa

    method post_press =
      lwt () = papa#post_press in
      let () = me#update_position in
      Lwt.return ()

    method update_position =
      (* The following code attempt to move the dropdown under the element
       * to which the dropdown is attached *)
      let computed_att = Ow_fun.getComputedStyle att' in
      let aw = Ow_size.get_full_width computed_att in
      let ah = Ow_size.get_full_height computed_att in
      let hshift, vshift =
        (* TODO: Display the dropdown on another place if the current
         * orientation put the dropdown outside of the screen *)
        match orientation with
        | `Bottom ->
          (Dom_html.document##body##scrollLeft),
          (ah + Dom_html.document##body##scrollTop)
        | `Right ->
          (Dom_html.document##body##scrollLeft + aw),
          (Dom_html.document##body##scrollTop)
        | `Left ->
          (Dom_html.document##body##scrollLeft - aw),
          (Dom_html.document##body##scrollTop)
      in
      let rectopt = att'##getClientRects()##item(0) in
      let (container_top, container_left) =
        Js.Opt.case rectopt
          (fun rect -> (Js.string "0px", Js.string "0px"))
          (fun rect ->
             let to_css shift x =
               let integer = (int_of_float (Js.to_float x)) + shift in
               Js.string (string_of_int integer^"px")
             in
             (to_css vshift rect##top, to_css hshift rect##left))
      in
      match node with
      | None -> ()
      | Some node ->
        node##style##top <- container_top;
        node##style##left <- container_left;
        let computed_node = Ow_fun.getComputedStyle node in
        let nodew = Ow_size.get_full_width computed_node in
        (* We want to remove the extra width of the element to get exactly
         * the same width of the attached element if his size is bigger
         * than the node's one *)
        let extra_inner =
          Ow_size.get_full_width ~with_width:false computed_node
        in
        if aw > nodew then
          node##style##width <-
            Js.string (string_of_int (aw - extra_inner)^"px")
        else ()

  end

}}

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

[%%client
  (*
  var event;
    if (document.createEvent) {
      event = document.createEvent("HTMLEvents");
      event.initEvent("dataavailable", true, true);
    } else {
      event = document.createEventObject();
      event.eventType = "dataavailable";
    }

    event.eventName = eventName;
    event.memo = memo || { };

    if (document.createEvent) {
      element.dispatchEvent(event);
    } else {
      element.fireEvent("on" + event.eventType, event);
    }
   *)

  class type ['a] customEventInit = object
    method bubbles : bool Js.t Js.prop
    method cancelable : bool Js.t Js.prop
    method detail : 'a Js.t Js.opt Js.prop
  end

  class type ['a] customEvent = object
    inherit Dom_html.event
    inherit ['a] customEventInit
  end

    type 'a ctor = (Js.js_string Js.t -> 'a customEventInit Js.t -> 'a customEvent Js.t) Js.constr

    let customEvent ?(can_bubble = false) ?(cancelable = false) ?(detail : 'a Js.t option) typ =
      let detail = match detail with
         | None -> Js.null
         | Some detail -> Js.some detail
      in
(*
      let ctor : 'a ctor =
        Js.Optdef.case (Js.def (Js.Unsafe.variable ("CustomEvent")))
          (fun () -> raise (Failure "CustomEvent is not supported")) (* TODO *)
          (fun ctor -> ctor)
      in
      let init = ((Js.Unsafe.obj [||]) : 'a customEventInit Js.t) in
      init##bubbles <- Js.bool can_bubble;
      init##cancelable <- Js.bool cancelable;
      init##detail <- detail;
      jsnew ctor (Js.string typ, init)
*)
      let ev = Js.Unsafe.global##.document##(createEvent (Js.string "CustomEvent")) in
      ev##(initCustomEvent
        (Js.string typ) (Js.bool can_bubble) (Js.bool cancelable) detail);
      ev

    let dispatchEvent (elt : #Dom_html.element Js.t) (ev : #Dom_html.event Js.t) : unit =
      (Js.Unsafe.coerce elt)##(dispatchEvent ev)
]

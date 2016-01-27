(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Christophe Lecointe
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


[%%client.start]
  (** Binding of the jQuery_ui slider for ocaml.

      To use it, you must include jquery-ui.js and jquery-1.9.1.js

      @author Christophe Lecointe
      @see < http://api.jqueryui.com/slider/ > JQueryUI slider.
  *)

  (** Add a slider to the given element.
      @param vertical Whether the slider is vertical or not. If it is not present, the slider will be horizontal.
      @param slide A function called during each slide.
  *)
  val add_slider : ?vertical:bool -> ?slide:(unit->unit) -> Dom_html.element Js.t -> unit

  (** Return the value of the slider. *)
  val get_value : Dom_html.element Js.t -> int

  (** Replace the callback function done on slides. *)
  (* TODOC Replace or add ? it add on the scrollbar module, better be consistant on this. *)
  val on_slide : Dom_html.element Js.t -> (unit -> unit) -> 'a Lwt.t


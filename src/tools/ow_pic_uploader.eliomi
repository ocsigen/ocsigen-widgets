(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
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

(**

   Widget to upload a picture, check it on server side, resize it,
   and possibly ask the user to crop it.

*)
(**

   Call function [make] for each picture uploader you want to create,
   with some parameters like the directory where you want to save the
   pictures.
   This will create a service and the widget to upload in this
   directory using this service.

   Be careful to set the right options in Ocsigen server's configuration file
   to be able to upload files and limit the maximum size of uploaded files.

*)

{shared{

(** The type of picture uploaders. *)
type t

}}

(** [make ~directory ~name ()] creates a picture uploader in [directory]
    with a service called [name] (choose the name you want, as lon g as two
    uploaders have different names).

    - If optional argument [crop_ratio] is present, the user will be asked
    to crop the picture. [None] means that no ratio will be forced.
    [Some  v] means that the widget will impose width / height = v.
    - Optional aguments [?max_height] and [?max_width] are used
    if you want the picture to be resized (for example to avoid large files).
    Images will never be enlarged.
    If the the image is to be cropped, the resizing will be made after cropping.
    - Argument [?continuation] contains the action to be executed
    after the picture is saved in the destination directory

*)
val make :
  directory: string list ->
  name: string ->
  ?crop_ratio: float option ->
  ?max_width: int ->
  ?max_height: int ->
  ?continuation:(string -> unit Lwt.t) ->
  unit ->
  t

{shared{
(** Creates a form that will ask for an image, send it, possibly ask te user
    to crop it. On success, the continuation called as last parameter will
    be called with the name of the file on server as parameter
    (use this for example to close the popup window).

    - Argument [~url_path] is the URL path of the directory containing
    these pictures on the server.
    - Argument [~text] is the text you want to display before the form.
    - Arguments [~err_log] and [std_log] are the client-side functions to use
    to display (error or information) messages. Example [Eba_msg.log]
    if you are using Eliom base app (as client values if you are using this
    from server side).

 *)
val upload_pic_form :
  t ->
  url_path: string list ->
  text: string ->
  err_log: (string -> unit) ->
  std_log: (string -> unit) ->
  (string -> unit Lwt.t) ->
  [ `Div ] Eliom_content.Html5.D.elt
}}

{client{

(** This function will display a popup asking the user to upload a picture,
    and return the name of the picture.
    The arguments are the same as for [upload_pic_form].
*)
val upload_pic_popup :
  t ->
  url_path: string list ->
  text: string ->
  err_log: (string -> unit) ->
  std_log: (string -> unit) ->
  unit ->
  string Lwt.t

 }}

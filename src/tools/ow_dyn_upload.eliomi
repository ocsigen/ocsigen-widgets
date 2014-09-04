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


(*VVV TODO: Doc intro. What is it? *)

{shared{
  (* SUGGESTIONS: Use this type as an abstract type for each
   * of the followings functions ? To enforce to use our [service]
   * function ? *)

  (** Type of a dynamic service. *)
  type dynup_service_t =
      (unit, Eliom_lib.file_info,
       Eliom_service.service_method, Eliom_service.non_attached_kind,
       [`NonattachedCoservice],
       [ `WithoutSuffix ], unit,
       [ `One of Eliom_lib.file_info ] Eliom_parameter.param_name,
       [ `Registrable ], string Eliom_service.ocaml_service)
        Eliom_service.service
}}

{shared{
  (** Exception raised in case of invalid extension *)
  exception Invalid_extension
}}

{server{
  (** Create a dynamic service used to upload files. See
      also [handler] and [register] *)
  val service : ?name:string -> unit -> dynup_service_t

  (* SUGGESTIONS:
   * - use a value instead of an hard coded string for "static" ? *)

  (** Handler associated to a (dyn_upload) [service]. You have to provide
      a function which will take the file name. This handler allows
      you to do some manipulation on the file which will be uploaded.

      You can provide some functions to custom the uploaded file. You
      can set a [timeout], enable the remove of unused file, using
      [remove_on_timeout]. You can also
      provide a function to generate a new filename ([new_filename]).

      You can also give a list of valid extensions. If a file, does not
      have a valid extension, and exception of type [Invalid_extension]
      will be raised.
     *)
  val handler :
     ?timeout:float
  -> ?remove_on_timeout:bool
  -> ?dir:(string list)
  -> ?new_filename:(unit -> string)
  -> ?extensions:string list
  -> (string -> unit Lwt.t)
  -> (unit -> Ocsigen_extensions.file_info -> string Lwt.t)

  (** Register a dynamic uploader service *)
  val register :
     dynup_service_t
  -> (unit -> Ocsigen_extensions.file_info -> string Lwt.t)
  -> unit

(*VVV What is it? *)
  val mark_as_used : string -> unit Lwt.t

}}

{client{
  (** [dyn_upload ~service ~file handler] will send the file
      using the service and call the handler with the server-side filename
      of [file]. *)
  val dyn_upload :
     service:dynup_service_t
  -> file:File.file Js.t
  -> (string -> unit Lwt.t)
  -> unit Lwt.t
}}

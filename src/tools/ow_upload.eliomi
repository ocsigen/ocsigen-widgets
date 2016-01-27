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

(**
   Helper function to upload files to a given directory, with possibly
   a timeout on their lifetime.
*)

(**
   Be careful to set the right options in Ocsigen server's configuration file
   to be able to upload files and limit the maximum size of uploaded files.
*)

[%%server.start]

  (** Call this function once at startup with parameters [~dir] and [()]
      (and possibly optional parameters) to initialize a directory where
      uploaded files will be stored.
      It returns a function to be called in upload services
      on file information, that saves the file in the directory
      with a new unique name, and returns this name.

      Personalize the file saver using the parameters of this function:
      - [~dir] is the absolute path of the directory
      where files are to be uploaded
      - If optional parameter [remove_on_timeout] is set to [true],
      the file will be deleted after an amount of time comprised
      between [timeout] and 2 × [timeout] (in seconds, default 180.).
      - The optional [?new_filename] parameter makes possible to
      customize generated file names. The default is based on
      [Ocsigen_lib.make_cryptographic_safe_string].
      - The optional [?cp] parameter is the function to be used
      to copy the file to its destination. By default, it is [Lwt_unix.link],
      which supposes the directory is on the same file system as the
      temporary upload directory set for Ocsigen server. Replace this
      function by something else if you want for example to perform
      some checks on the file or to transform it (resize images, etc.)

     *)
  val create_file_saver :
    directory:(string list)
    -> ?timeout:float
    -> ?remove_on_timeout:bool
    -> ?new_filename:(string -> string)
    -> ?cp:(string -> string -> unit Lwt.t)
    -> unit
    -> (Ocsigen_extensions.file_info -> string Lwt.t)

  (** The default function for generating new file names.
      It takes the temporary filename as parameter. *)
  val default_new_filename : string -> string



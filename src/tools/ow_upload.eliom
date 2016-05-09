(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Charly Chevalier
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

[%%shared
  open Eliom_content
  open Eliom_content.Html

]

[%%server
  let mkdir_if_needed dirs =
    try
      ignore (List.fold_left
                (fun prev_dir dir ->
                   let path = prev_dir^"/"^dir in
                   if not (Sys.file_exists path)
                   then Unix.mkdir path 0o766;
                   path)
                (List.hd dirs)
                (List.tl dirs))
    with
    | Failure _ -> ()
    | Unix.Unix_error _ as exn ->
      Lwt_log.ign_log
        ~exn ~section:Ow_log.section ~level:Lwt_log.Error
        "Error in Ow_upload.mkdir_if_needed"

]

[%%server

  let default_new_filename _ =
    let base64url_of_base64 s =
      for i = 0 to String.length s - 1 do
        if s.[i] = '+' then s.[i] <- '-' ;
        if s.[i] = '/' then s.[i] <- '_' ;
      done
    in
    let fname = Ocsigen_lib.make_cryptographic_safe_string () in
    base64url_of_base64 fname;
    fname

  (* This function acts like a daemon.
     It removes the files older than [timeout] seconds,
     every [timeout] seconds (means that files may last at most
     2 × [timeout]). *)
  let clean_temporary_dir dpath timeout =
    let rec loop () =
      let filenames = Sys.readdir dpath in
      let now = Unix.gettimeofday () in
      let%lwt () = Array.fold_left
        (fun t filename ->
           let%lwt () = t in
           let filename = String.concat "/" [dpath; filename] in
           let%lwt stat = Lwt_unix.LargeFile.stat filename in
           let date = stat.Lwt_unix.LargeFile.st_mtime in
           if now -. date >= timeout
           then Lwt_unix.unlink filename
           else Lwt.return ()
        )
        (Lwt.return ())
        filenames
      in
      let%lwt () = Lwt_unix.sleep timeout in
      loop ()
    in
    try%lwt
      loop ()
    with
      | Unix.Unix_error _ as exn ->
        Lwt_log.log
          ~exn ~section:Ow_log.section ~level:Lwt_log.Error
          "Error in Ow_upload.clean_temporary_dir"

  let create_file_saver
    ~directory
    ?(timeout = 180.)
    ?(remove_on_timeout = false)
    ?(new_filename = default_new_filename)
    ?(cp = Lwt_unix.link)
    ()
    =
    let dpath = String.concat "/" directory in
    let () = mkdir_if_needed directory in
    (* Now launching the file cleaner, if requested: *)
    if remove_on_timeout
    then (* We launch a thread which will scan the directory
            and delete files on timeout *)
      Lwt.async (fun () -> clean_temporary_dir dpath timeout);
    fun file_info ->
      let fname = new_filename (Eliom_request_info.get_tmp_filename file_info)
      in
      let fpath = String.concat "/" [dpath; fname] in
      let%lwt () = cp (Eliom_request_info.get_tmp_filename file_info) fpath in
      Lwt.return fname

]

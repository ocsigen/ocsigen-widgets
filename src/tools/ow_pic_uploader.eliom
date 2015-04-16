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

{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F

type crop_type =
  string (* image name *) *
 (float * float * float * float)
  deriving (Json)

type t =
  { directory : string list;
    service : (unit, Eliom_lib.file_info,
               [ Eliom_service.service_method ],
               [ Eliom_service.attached ],
               [ Eliom_service.service_kind ],
               [ `WithoutSuffix ], unit,
               [ `One of Eliom_lib.file_info ] Eliom_parameter.param_name,
               [ Eliom_service.registrable ],
               string Eliom_service.ocaml_service)
        Eliom_service.service;
    crop : ((crop_type, unit) server_function * float option) option;
  }
}}

let resize im ?max_width ?max_height () =
  let height = Magick.get_image_height im in
  let width = Magick.get_image_width im in
  let resize_ratio_h =
    Eliom_lib.Option.map
      (fun maxh -> (float_of_int height) /. (float_of_int maxh))
      max_height
  in
  let resize_ratio_w =
    Eliom_lib.Option.map
      (fun maxw -> (float_of_int width) /. (float_of_int maxw))
      max_width
  in
  let resize_ratio = match resize_ratio_w, resize_ratio_h with
    | None, None -> None
    | a, None -> a
    | None, a -> a
    | Some a, Some b -> Some (max a b)
  in
  match resize_ratio with
  | None -> ()
  | Some r when r <= 1. -> ()
  | Some r ->
    let height = int_of_float ((float_of_int height) /. r) in
    let width = int_of_float ((float_of_int width) /. r) in
    Magick.Imper.resize im ~width ~height
      ~filter:Magick.Point
      ~blur:0.0

let check_and_resize ?max_height ?max_width () name1 name2 =
  Lwt_preemptive.detach
    (fun () ->
       (* Even if no resizing is requested, we read the image,
          in order to check it. *)
       let im = Magick.read_image ~filename:name1 in
       resize im ?max_height ?max_width ();
       Magick.write_image im ~filename:name2)
    ()

let crop_and_resize
    name1 name2
    crop_ratio ?max_width ?max_height
    (x, y, width, height) =
  (* Magick is not cooperative. We use a preemptive thread *)
  Lwt_preemptive.detach
    (fun () ->
       let im = Magick.read_image ~filename:name1 in
       (* If the crop ratio is fixed, we do not trust the height sent by
          the client. We recompute it. *)
       let height = match crop_ratio with
         | None -> int_of_float height
         | Some ratio -> int_of_float (width /. ratio)
       in
       let width = int_of_float width in
       let x = int_of_float x in
       let y = int_of_float y in
       Magick.Imper.crop im ~x ~y ~width ~height;
       resize im ?max_height ?max_width ();
       Magick.write_image im ~filename:name2)
    ()

let make_crop_handler ~directory ~crop_ratio ?max_width ?max_height () =
  let dest_path = String.concat "/" directory in
  let src_path = String.concat "/" ([dest_path; "tmp"]) in
  fun (fname, coord) ->
    let src = String.concat "/" ([src_path; fname]) in
    let dest = String.concat "/" ([dest_path; fname]) in
    crop_and_resize src dest crop_ratio ?max_width ?max_height coord

let new_filename filename =
  let name = Ow_upload.default_new_filename filename in
  name^".jpg"

let make ~directory ~name ?crop_ratio ?max_width ?max_height
    ?(service_wrapper = fun f a -> f a)
    ?(crop_wrapper = fun f a -> lwt _ = f a in Lwt.return ())
    () =
  let service = Eliom_service.Ocaml.post_coservice'
      ~name
      ~post_params:(Eliom_parameter.file "f")
      ()
  in
  let service_handler, crop = match crop_ratio with
    | None -> (* No crop. We just save the picture in the destination
                 directory, without timeout, after checking the file
                 and resizing it. *)
      let cp = check_and_resize ?max_height ?max_width () in
      let service_handler =
        Ow_upload.create_file_saver ~new_filename ~directory ~cp ()
      in
      service_handler, None
    | Some crop_ratio -> (* We want to ask the user to crop the picture *)
      (* In that case, we save the picture in a temporary directory
         with a timeout. *)
      let service_handler = Ow_upload.create_file_saver
          ~directory:(directory@["tmp"])
          ~timeout:600.
          ~new_filename
          ~remove_on_timeout:true
          ()
      in
      (* We define a new service for crop coordinates.
         This could be a dynamic coservice
         but as nobody knows the name of the picture at this time,
         we use a static service, hidden in a server_function. *)
      let crop_handler = make_crop_handler ~directory ~crop_ratio () in
      let crop_fun = server_function
          ~name:("_c"^name)
          Json.t<crop_type>
          (crop_wrapper
             (fun ((fname, _) as v) ->
                lwt () = crop_handler v in
                Lwt.return fname))
      in
      (service_handler, Some (crop_fun, crop_ratio))
  in
  Eliom_registration.Ocaml.register service
    (fun () -> service_wrapper service_handler);
  { directory;
    service;
    crop }


{client{

   let bind_send_button
       uploader
       url_path inp send_button container on_error continuation =
     Lwt_js_events.async (fun () ->
       Lwt_js_events.clicks (To_dom.of_element send_button)
         (fun _ _ ->
            Js.Optdef.case ((To_dom.of_input inp)##files)
              (fun _ -> Lwt.return ())
              (fun files ->
                 Js.Opt.case (files##item(0))
                   (fun () -> Lwt.return ())
                   (fun file ->
                      Manip.removeChildren container;
                      Manip.appendChild container (Ow_icons.F.spinner ());
                      try_lwt
                        lwt fname =
                          Eliom_client.call_ocaml_service
                            ~service:uploader.service
                            () file
                        in
                        match uploader.crop with
                        | None -> (* Finished! *)
                          continuation fname
                        | Some (crop_fun, crop_ratio) ->
                          let im = D.img ~alt:"image to be cropped"
                              ~src:(make_uri
                                      ~service:(Eliom_service.static_dir ())
                                      (url_path@["tmp"; fname]))
                              ()
                          in
                          let send_button =
                            D.Raw.input
                              ~a:[a_input_type `Submit; a_value "Crop"] ()
                          in
                          Manip.removeChildren container;
                          Manip.appendChild container
                            (p [pcdata "Select an area of the picture"]);
                          Manip.appendChild container im;
                          Manip.appendChild container send_button;
                          let coord = ref (0., 0., 100., 100.) in
                          Lwt.async (fun () ->
                            (* We must wait for the image to be loaded
                               before setting the crop widget *)
                            let im' = To_dom.of_img im in
                            lwt _ = Lwt_js_events.load im' in
                            let nw = Js.Optdef.case (im'##naturalWidth)
                                (fun () -> 0) (fun x -> x)
                            in
                            let nh = Js.Optdef.case (im'##naturalHeight)
                                (fun () -> 0) (fun x -> x)
                            in
                            let side = min (nw / 3) (nh / 3) in
                            let hside = side / 2 in
                            let (x1, y1) = (nw / 2 - hside, nh / 2 - hside) in
                            let (x2, y2) = (x1 + side, y1 + side) in
                            let set_select = (x1, y1, x2, y2) in
                            coord := (float x1, float y1,
                                      float (x2 - x1),
                                      float (y2 - y1));
                            ignore
                              (new Ow_jcrop.jcrop
                                ?aspect_ratio:crop_ratio
                                ~set_select
                                ~true_size:(nw, nh)
                                ~on_select:(fun c ->
                                  coord := (c##x,c##y,c##w,c##h))
                                ~allow_select:false
                                (To_dom.of_img im));
                            Lwt.return ());
                          Lwt_js_events.clicks (To_dom.of_element send_button)
                            (fun _ _ ->
                               Manip.removeChildren container;
                               Manip.appendChild container
                                 (Ow_icons.F.spinner ());
                               try_lwt
                                 lwt () = crop_fun (fname, !coord) in
                                 continuation fname
                               with e -> on_error e)
                      with e -> on_error e)
              )
         )
     )


 }}

{shared{

let upload_pic_form t ~url_path ~text ~on_error ~continuation () =
  let inp = D.Raw.input ~a:[a_input_type `File] () in
  let send_button = D.Raw.input ~a:[a_input_type `Submit; a_value "Send"] () in
  let container = D.div ~a:[a_class ["ow_pic_uploader"]] [ inp; send_button ] in
  ignore {unit{
    bind_send_button
      %t %url_path %inp %send_button %container %on_error %continuation }};
  container

 }}

{client{

let upload_pic_popup t ~url_path ~text () =
  let w, u = Lwt.wait () in
  let box = ref None in
  let continuation fname =
    Eliom_lib.Option.iter Manip.removeSelf !box;
    Lwt.wakeup u (Some fname);
    Lwt.return ()
  in
  let on_error e =
    Eliom_lib.Option.iter Manip.removeSelf !box;
    Lwt.wakeup_exn u e;
    Lwt.return ()
  in
  let close_button = Ow_icons.D.close () in
  Lwt_js_events.async (fun () ->
    Lwt_js_events.clicks (To_dom.of_element close_button)
      (fun _ _ ->
         Eliom_lib.Option.iter Manip.removeSelf !box;
         Lwt.wakeup u None;
         Lwt.return ()
      ));
  let form = upload_pic_form t ~url_path ~text ~on_error ~continuation () in
  let d = D.div ~a:[a_class ["ow_background"]]
      [div ~a:[a_class ["ow_popup"]] [close_button; form]]
  in
  box := Some d;
  Manip.appendToBody d;
  w


}}

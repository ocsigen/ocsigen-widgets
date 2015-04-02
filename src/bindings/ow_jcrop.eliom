(*
   Copyright (c) 2011 Tapmodo Interactive LLC,
   http://github.com/tapmodo/Jcrop

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
 *)

(*
   Author: Charly Chevalier
   *)

{client{
  open Ow_pervasives

  (* Type used to constraint usage of parameters of Jcrop's callbacks *)
  class type param = object
    method x : float Js.readonly_prop
    method x2 : float Js.readonly_prop
    method y : float Js.readonly_prop
    method y2 : float Js.readonly_prop
    method h : float Js.readonly_prop
    method w : float Js.readonly_prop
  end

  (* Object to simulate Jcrop parameters on creation of the widget *)
  class type options = object
    method aspectRatio : float Js.writeonly_prop
    method minSize : int Js.js_array Js.t Js.writeonly_prop
    method maxSize : int Js.js_array Js.t Js.writeonly_prop
    method trueSize : int Js.js_array Js.t Js.writeonly_prop
    method boxHeight : int Js.writeonly_prop
    method boxWidth : int Js.writeonly_prop
    method setSelect : int Js.js_array Js.t Js.writeonly_prop
    method allowSelect : bool Js.t Js.writeonly_prop
    method bgColor : string Js.writeonly_prop
    method bgOpacity : float Js.writeonly_prop
    method onSelect : (param Js.t -> unit) Js.callback Js.writeonly_prop
    method onChange : (param Js.t -> unit) Js.callback Js.writeonly_prop
    method onRelease : (param Js.t -> unit) Js.callback Js.writeonly_prop
  end

  class jcrop
    ?on_select
    ?on_change
    ?on_release
    ?aspect_ratio
    ?min_size
    ?max_size
    ?box_height
    ?box_width
    ?true_size
    ?set_select
    ?(allow_select = true)
    ?(bg_color = "black")
    ?(bg_opacity = 0.6)
    (elt : Dom_html.imageElement Js.t)
    =
  let wrap_callback cb =
    Js.wrap_callback
      (fun c ->
         cb ((Js.Unsafe.coerce c) :> param Js.t))
  in
  object(self)
    initializer
    (* Create default object for options *)
    let opt = (Js.Unsafe.obj [||] :> options Js.t) in

    Ow_option.iter (fun ov -> opt##onSelect <- wrap_callback ov) on_select;
    Ow_option.iter (fun ov -> opt##onChange <- wrap_callback ov) on_change;
    Ow_option.iter (fun ov -> opt##onRelease <- wrap_callback ov) on_release;

    Ow_option.iter (fun ov -> opt##aspectRatio <- ov) aspect_ratio;
    Ow_option.iter (fun (x1, y1, x2, y2) ->
      opt##setSelect <-  Js.array [| x1; y1; x2; y2 |]) set_select;
    Ow_option.iter (fun (x, y) -> opt##minSize <- Js.array [| x; y |]) min_size;
    Ow_option.iter (fun (x, y) -> opt##maxSize <- Js.array [| x; y |]) max_size;
    Ow_option.iter (fun ov -> opt##boxHeight <- ov) box_height;
    Ow_option.iter (fun ov -> opt##boxWidth <- ov) box_width;
    Ow_option.iter
      (fun (x, y) -> opt##trueSize <- Js.array [| x; y |]) true_size;

    opt##allowSelect <- Js.bool allow_select;
    opt##bgColor <- bg_color;
    opt##bgOpacity <- bg_opacity;

    let img =
      Js.Unsafe.coerce (Ojquery.js_jQelt (elt :> Dom_html.element Js.t))
    in
    img##_Jcrop(opt)
  end
}}

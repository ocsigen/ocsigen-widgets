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
  open Eliom_content.Html5
  open Html5_types
]
[%%client
  open Dom_html
  open Dom
]


[%%client
  module M = Regexp

  (* word is a completion of [pattern]. ex: is_completed_by ~pattern:"e" "eddy" = yes *)
  (* both arg are utf16 JS string *)
  let is_completed_by
      ?(accents = false)
      ?(sensitive = true)
      ?(from_start = false)
      ~pattern w =
    let word_match w =
      let pattern, w =
        if accents then begin
          Js.to_string (Ow_fun.removeDiacritics pattern),
          Js.to_string (Ow_fun.removeDiacritics w)
        end else (Js.to_string pattern, Js.to_string w)
      in
      let regex =
        let pattern = M.quote pattern in
        let regex =
          if from_start
          then ("^" ^ pattern) ^ "|^\\s" ^ pattern
          else (".*" ^ pattern ^ ".*")
        in
        if sensitive
        then M.regexp regex
        else M.regexp_with_flag regex "i"
      in
      match M.search regex w 0 with
      | None -> false
      | Some _ -> true
    in
    if (Js.to_string pattern) = (Js.to_string w) then true
    else (word_match w)
]

[%%client
  class type completion = object
    inherit Ow_dropdown.dropdown

    method value : Js.js_string Js.t Js.prop

    method clear : unit Js.meth
    method confirm : unit Lwt.t Js.meth
    method refresh : unit Lwt.t Js.meth
  end

  class type completion' = object
    inherit completion

    method _choices : Html5_types.li elt list Js.prop
    method _needUpdate : bool Js.prop
    method _selected : bool Js.prop
    method _selectedValue : string Js.prop
    method _oldValue : string Js.prop

    method _confirm : (#completion Js.t, unit Lwt.t) Js.meth_callback Js.prop

    method _clear : (#completion Js.t, unit) Js.meth_callback Js.prop
    method _confirm : (#completion Js.t, unit Lwt.t) Js.meth_callback Js.prop
    method _refresh : (#completion Js.t, unit Lwt.t) Js.meth_callback Js.prop
  end

  let default_on_confirm _ =
    Lwt.return ()

  let completion
        ~refresh
        ?(limit = 5)
        ?(accents = false)
        ?(from_start = false)
        ?(force_refresh = false)
        ?(sensitive = false)
        ?(adaptive = true)
        ?(auto_match = true)
        ?(clear_input_on_confirm = true)
        ?(move_with_tab = false)
        ?(on_confirm = default_on_confirm)
        elt elt_traversable =
    let elt' = (Js.Unsafe.coerce (To_dom.of_element elt) :> completion' Js.t) in
    let meth = Js.wrap_meth_callback in

    elt'##.classList##(add (Js.string "ojw_completion"));

    let get_data_value_to_match n =
      (Js.Unsafe.coerce n)##(getAttribute (Js.string "data-value-to-match"))
    in

    let get_data_value n =
      (Js.Unsafe.coerce n)##(getAttribute (Js.string "data-value"))
    in

    let set_input v = elt'##.value := v in
    let set_selected_value v = elt'##._selectedValue := v in

    (* Returns [true] if there is an active element and
       if it has a data-value-to-display attribute *)
    let set_input_and_set_selected_value_with_active () =
      let elt_traversable' = Ow_traversable.to_traversable elt_traversable in
      Js.Opt.iter (elt_traversable'##getActive)
        (fun act ->
           let act' = To_dom.of_li act in
           Js.Opt.iter (get_data_value_to_match act') set_input;
           Js.Opt.iter (get_data_value act') set_selected_value)
    in
    (* Set the first element (if one) of the dropdown as selected. *)
    let set_first_as_selected () =
      let elt_traversable' = To_dom.of_ul elt_traversable in
      Js.Opt.iter (elt_traversable'##.firstChild)
        (fun first ->
           (Ow_traversable.to_traversable elt_traversable)
           ##(setActive (Of_dom.of_li (Js.Unsafe.coerce first))));
    in
    (* Set the input value to the data-value-to-display of the selected element. *)
    let set_as_selected () =
      set_input_and_set_selected_value_with_active ();
      elt'##clear;
      elt'##._needUpdate := false;
      elt'##._selected := true;
    in
    (* Reset the context of the completion widget.
       - _needUpdate: indicates whether or not the [refresh] function (parameter)
       should be called during the next action.
       - _selected: indicates if an element has been selected (by pressing
       enter/tab)
       - #clear(): will removes all the nodes of the completion widgets.
       *)
    let reset_context () =
      Ow_log.log "reset_context";
       elt'##._needUpdate := true;
       elt'##._selected := false;
       elt'##._choices := [];
    in
    (* This function will be used by the [traversable] widget. [true] means
       that the event will be prevented. *)
    let on_keydown e =
      let elt_traversable' = To_dom.of_ul elt_traversable in
      let has_content = elt_traversable'##.childNodes##.length <> 0 in
      match e##.keyCode with
        | 9 -> (* tab (without shift) *)
            if not move_with_tab && has_content then begin
              set_as_selected ();
              Lwt.return true
            end
            else if move_with_tab then Lwt.return true
            else Lwt.return false
        | _ -> Lwt.return false
    in

    (* Partial application of [is_completed_by] (bind some arguments). *)
    let is_completed_by = is_completed_by ~sensitive ~accents ~from_start in

    (* Epurate the given list using [is_completed_by]. [on_fail] will be
       called when an item does not match. *)
    let epur ?on_fail ~get_attr l =
      let value = elt'##.value in
      let rec aux rl = function
        | [] -> rl
        | hd::tl ->
            Js.Opt.case (get_attr hd)
              (fun () -> aux rl tl)
              (fun data_value ->
                 if is_completed_by ~pattern:value data_value
                 then aux (hd::rl) tl
                 else begin
                   (match on_fail with
                    | None -> ()
                    | Some on_fail -> on_fail hd);
                   aux rl tl
                 end)
      in aux [] l
    in
    (* Indicates when a dropdown content (list of li under the input) must be
       shown or not and also indicates when keydowns events should be handled
       or not. *)
    let is_traversable dd =
      Js.to_bool dd##.pressed || dd##.traversable##.childNodes##.length > 0
    in
    (* .. *)
    let predicate () = Lwt.return false in

    ignore (
      Ow_dropdown.dropdown
        ~focus:false
        ~on_keydown
        ~enable_link:false
        ~is_traversable
        ~predicate
        elt
        elt_traversable
    );

    elt'##._refresh :=
    meth (fun this ->
      let elt_traversable' = To_dom.of_ul elt_traversable in
      let value = elt'##.value in
      let%lwt rl =
        let%lwt rl = refresh limit (Js.to_string value) in
        elt'##._choices := rl;
        if not auto_match
        then Lwt.return (rl)
        else Lwt.return
            (epur ~get_attr:(fun a -> get_data_value_to_match (To_dom.of_li a)) rl)
      in
      let mapn n f l =
        if n < 0 then failwith "limit, invalid argument";
        let rec aux rl = function
          | 0, _  -> rl
          | _, [] -> rl
          | n, hd::tl -> aux (f hd::rl) ((n-1), tl)
        in aux [] (n,l)
      in
      List.iter
        (Dom.appendChild elt_traversable')
        (mapn limit To_dom.of_li rl);
      set_first_as_selected ();
      elt'##._needUpdate := false;
      Lwt.return ()
    );

    let soft_refresh () =
      let elt_traversable' = To_dom.of_ul elt_traversable in
      let selected_has_been_removed = ref false in
      let remove_and_reset elt =
        let elt' = Js.Unsafe.coerce elt in
        let selected = Js.string "selected" in
        if elt'##.classList##(contains selected) then begin
          elt'##.classList##(remove selected);
          selected_has_been_removed := true
        end;
        Dom.removeChild elt_traversable' elt'
      in
      let el =
        (* The list of epured items. Those which didn't match the input
           value have been removed during [epur] function. *)
        epur ~on_fail:remove_and_reset ~get_attr:get_data_value_to_match
          (Dom.list_of_nodeList (elt_traversable'##.childNodes))
      in
      let is_same node =
        List.exists (fun n -> (get_data_value_to_match n) = (get_data_value_to_match node)) el
      in
      let value = elt'##.value in
      (* We can use the epured items to insert only nodes which are not already
         inserted. *)
      let rec soft_refresh n = function
        | [] -> ()
        | hd::tl ->
            if n >= limit then ()
            else begin
              let hd' = To_dom.of_li hd in
              Js.Opt.case (get_data_value_to_match hd')
                (fun () -> soft_refresh n tl)
                (fun data_value ->
                   if not (is_same hd')
                   && is_completed_by ~pattern:value data_value then begin
                     Dom.appendChild elt_traversable' hd';
                     soft_refresh (n+1) tl
                   end else soft_refresh n tl)
            end
      in soft_refresh (List.length el) (elt'##._choices);
      (* If we remove the element which was selected, so we're going to
         select the first element as default one. *)
      if !selected_has_been_removed then
        set_first_as_selected ()
    in

    elt'##._clear :=
    meth (fun this ->
       let elt_traversable' = To_dom.of_ul elt_traversable in
       reset_context ();
       List.iter
         (Dom.removeChild elt_traversable')
         (Dom.list_of_nodeList (elt_traversable'##.childNodes))
    );

    elt'##._confirm :=
    meth (fun this ->
       let selected_value = elt'##._selectedValue in
       if clear_input_on_confirm then begin
         set_input (Js.string "");
         elt'##._oldValue := "";
       end;
       elt'##clear;
       on_confirm selected_value;
    );

    elt'##._oldValue := "";
    reset_context ();

    Lwt.async (fun () ->
      Ow_traversable.actives elt_traversable
        (fun e _ ->
           Js.Opt.iter (e##.detail)
             (fun detail ->
                (match detail##by with
                 | `Click ->
                     set_input_and_set_selected_value_with_active ();
                     (* Re-give the focus to the input after a click *)
                     ((Js.Unsafe.coerce (To_dom.of_element elt)))##focus
                 | _ -> ()));
           Lwt.return ()));

    Lwt.async (fun () ->
      Lwt_js_events.focuses (To_dom.of_element elt)
        (fun _ _ ->
           elt'##press;
           Lwt.return ()));

    Lwt.async (fun () ->
      Lwt_js_events.blurs (To_dom.of_element elt)
        (fun _ _ ->
           elt'##unpress;
           Lwt.return ()));

    (* Using keyups make sure to capture the very first character of the input
       when it is empty. (keydowns is trigger before the input value changed)
     *)
    Lwt.async (fun () ->
      Lwt_js_events.keyups (To_dom.of_element elt)
        (fun e _ ->
           let keycode = e##.keyCode in
           (* Ignore up and down to prevent clear and refresh *)
           if keycode = 38 || keycode = 40 then Lwt.return ()
           else begin
             let elt_traversable' = Ow_traversable.to_traversable elt_traversable in
             (* delete/backspace: reset_context, we're going to update the
                dropdown content. *)
             (* If the value of the input is empty, we reset the context. *)
             let value = Js.to_string elt'##.value in
             Ow_log.log ("old:"^elt'##._oldValue);
             Ow_log.log ("cur:"^value);
             let value_has_bigger_length =
               String.length elt'##._oldValue > String.length value
             in
             if value_has_bigger_length then
               reset_context ();
             let has_content = elt_traversable'##.childNodes##.length <> 0 in
             Ow_log.log_int e##.keyCode;
             let keycode =
               if move_with_tab then match e##.keyCode with
                 | 9 when (Js.to_bool e##.shiftKey) ->
                     elt_traversable'##prev;
                     38
                 | 9 ->
                     elt_traversable'##next;
                     40
                 | _ as k -> k
               else e##.keyCode
             in
             let%lwt prevent = match keycode with
               | 13 -> (* enter *)
                   if not has_content || elt'##._selected then
                     let%lwt () = elt'##confirm in
                     Lwt.return false
                   else begin
                     set_as_selected ();
                     Lwt.return true
                   end
               | 38 | 40 -> (* up/down *)
                   if adaptive then begin
                     set_input_and_set_selected_value_with_active ();
                     Lwt.return true
                   end else Lwt.return false
               | _ -> Lwt.return false
             in
             if prevent then
               Dom.preventDefault e;
             (* Get the new value of the input value. Maybe it has been cleared
                *)
             let value = Js.to_string elt'##.value in
             (* We store the current value to compare it next time. We use
                keyups to catch the last character entered. (keydowns is trigger
                before the input value changed)
                *)
             elt'##._oldValue := value;
             if value = ""
             then (elt'##clear; Lwt.return ())
             else if (not elt'##._selected) then begin
               if force_refresh || elt'##._needUpdate then begin
                 elt'##clear;
                 elt'##refresh;
               end else Lwt.return (soft_refresh ());
             end else Lwt.return ()
           end));

    (elt , elt_traversable)
]

[%%server
  module M = Netstring_pcre

  let search rex w =
    try Some (M.search_forward rex w 0) with
      | Not_found -> None

  let regex_case_insensitive =
    M.regexp_case_fold
]

[%%server
  let build_pattern w =
    let w = M.quote w in
    regex_case_insensitive  (("^" ^ w) ^ "|\\s" ^ w)

  let search_case_insensitive w0 w1 =
    if w0 = "" || w0 = w1
    then None
    else
      let pattern = (build_pattern w0) in
      match search pattern w1 with
        | None -> None
        | Some (i,r) -> if i = 0 then Some (i,r) else Some (i+1, r)
]

[%%server
  (* arguments are utf8 caml string *)
  let search_case_accents_i w0 w1 =
    let w0 = Ow_accents.without w0 in
    let w1 = Ow_accents.without w1 in
    search_case_insensitive w0 w1
]

[%%server
  let searchopt_to_bool w0 w1 =
    match search_case_accents_i w0 w1 with
      | None -> false
      | Some _ -> true
]

[%%server
  (* w1 is a completion of w0. ex: is_completed_by "e" "eddy" = yes *)
  (* both arg are utf8 caml string *)
  let is_completed_by w0 w1 =
    if w0 = "" || w1 = ""
    then false
    else searchopt_to_bool w0 w1
]

[%%shared
  type refresh_fun' = int -> string -> li elt list Lwt.t
  type on_confirm_fun' = string -> unit Lwt.t
]

[%%shared
  let li ?a ~value ~value_to_match =
    Ow_traversable.li ?a ?href:None ~anchor:false ~value ~value_to_match
]

[%%server
  let completion
      ~(refresh : refresh_fun' client_value)
      ?(limit : int option)
      ?(accents : bool option)
      ?(sensitive : bool option)
      ?(adaptive : bool option)
      ?(auto_match : bool option)
      ?(clear_input_on_confirm : bool option)
      ?(move_with_tab : bool option)
      ?(on_confirm : on_confirm_fun' client_value option)
      (elt : 'a elt)
      (elt_traversable : ul elt) =
    ignore [%client (
        let (_,_) =
          completion
            ~refresh:~%refresh
            ?limit:~%limit
            ?accents:~%accents
            ?sensitive:~%sensitive
            ?adaptive:~%adaptive
            ?auto_match:~%auto_match
            ?clear_input_on_confirm:~%clear_input_on_confirm
            ?move_with_tab:~%move_with_tab
            ?on_confirm:~%on_confirm
          ~%elt
          ~%elt_traversable
        in ()
    : unit)];
    (elt, elt_traversable)
]

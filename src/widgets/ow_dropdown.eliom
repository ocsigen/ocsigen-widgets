{shared{
  open Eliom_content.Html5
  open Html5_types
  open Ow_dom
}}
{client{
  open Dom_html
  open Dom
}}

{client{
  class type dropdown = object
    inherit Ow_button.button

    method traversable : Ow_traversable.traversable Js.t Js.readonly_prop
  end

  class type dropdown' = object
    inherit dropdown

    method _timeout : unit Lwt.t Js.opt Js.prop
    method _traversable : Ow_traversable.traversable Js.t Js.prop
  end

  let dropdown
        ?(v = `bottom)
        ?(h = `center)
        ?(focus = true)
        ?(hover = false)
        ?(hover_timeout = 1.0)
        ?(enable_link)
        ?(is_traversable)
        ?(predicate)
        ?(on_keydown)
        elt elt_traversable =
    let elt' = (Js.Unsafe.coerce (Ow_button.to_button elt) :> dropdown' Js.t) in
    let elt_traversable' = to_ul_elt elt_traversable in

    (* Don't use the 'this' argument because it correspond to dropdown content
       and not the button used by the dropdown.

       FIXME: Should we check if 'pressed' method is not undefined ? It should
       never happen.. *)
    let is_traversable = match is_traversable with
      | None -> (fun _ -> Js.to_bool (elt'##pressed))
      | Some f -> (fun _ -> f (Js.Unsafe.coerce elt') (* FIXME why do we need to Unsafe.coerce ? *))
    in

    let on_mouseovers, on_mouseouts =
      (fun f ->
         Js.Opt.iter (elt'##_timeout)
           (fun th -> Lwt.cancel th);
         f ()),
      (fun () ->
         let th = Lwt_js.sleep hover_timeout in
         elt'##_timeout <- Js.some th;
         try_lwt
           lwt () = th in
           if (Js.to_bool elt'##pressed) then
             elt'##unpress();
           Lwt.return ()
         with Lwt.Canceled -> Lwt.return ())
    in

    let cstyle = Ow_fun.getComputedStyle elt' in
    elt_traversable'##style##minWidth <- cstyle##width;

    elt'##classList##add(Js.string "ojw_dropdown");

    ignore (
      Ow_button.button_alert
        ~pressed:false
        ?predicate
        ~v ~h
        elt elt_traversable
    );

    elt'##_traversable <-
      Ow_traversable.to_traversable
        (Ow_traversable.traversable
           ?on_keydown
           ?enable_link
           ~is_traversable
           ~focus
           elt_traversable
        );

    if hover then begin
      Lwt.async (fun () ->
          Lwt_js_events.mouseovers elt_traversable'
            (fun _ _ ->
               on_mouseovers (fun () -> ());
               Lwt.return ()));

      Lwt.async (fun () ->
          Lwt_js_events.mouseouts elt_traversable'
            (fun _ _ ->
               lwt () = on_mouseouts () in
               Lwt.return ()));
    end;

    elt'##_timeout <- Js.null;

    if hover then begin
      Lwt.async (fun () ->
        Lwt_js_events.mouseovers elt'
          (fun _ _ ->
             on_mouseovers (fun () ->
               if not (Js.to_bool elt'##pressed) then
                 elt'##press()
             );
             Lwt.return ()));

      Lwt.async (fun () ->
        Lwt_js_events.mouseouts elt'
          (fun _ _ ->
             lwt () = on_mouseouts () in
             Lwt.return ()));
    end;

    (elt, elt_traversable)
}}

{shared{
  let li ?a ~href = Ow_traversable.li ?a ?value:None ~anchor:true ~href ?value_to_match:None
}}

{server{
  let dropdown
      ?(v:Ow_position.v_orientation' option)
      ?(h:Ow_position.h_orientation' option)
      ?(hover:bool option)
      ?(hover_timeout:float option)
      (elt : 'a elt)
      (elt_traversable : ul elt) =
    ignore {unit{
      Eliom_client.onload (fun () ->
        ignore (
          dropdown
            ?v:%v
            ?h:%h
            ?hover:%hover
            ?hover_timeout:%hover_timeout
            %elt %elt_traversable
        )
      )
    }};
    (elt, elt_traversable);
}}

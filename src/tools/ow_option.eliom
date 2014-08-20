{shared{
  open Ow_pervasives

  (* FIXME
   * It would be MUCH better to take the value as first parameter
   * and the function to apply as second. Like Js.Opt
   *
   * *)

  let map f v =
    match v with
      | None -> None
      | Some a -> Some (f a)

  let map_lwt f v =
    match v with
      | None -> Lwt.return None
      | Some a -> Lwt.bind (f a) (fun r -> Lwt.return (Some r))

  let iter f v =
    match v with
      | None -> ()
      | Some a -> f a

  let iter_lwt f v =
    match v with
      | None -> Lwt.return ()
      | Some a -> f a

  let lwt_map t f g =
    match Lwt.state t with
      | Lwt.Return v -> f v
      | _ -> g ()
}}

(* FIXME
 * Should be of_jsOpt or of_js_opt ?
 *
 * *)
{client{
  let of_opt elt =
    Js.Opt.case elt (fun () -> failwith "of_opt") id
}}

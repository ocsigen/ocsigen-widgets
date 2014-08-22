{shared{
  type t' = int
}}

{client{
  class type item = object
    inherit Dom_html.element

    method enable : unit Js.meth
    method disable : unit Js.meth
  end

  class type item' = object
    inherit item

    method _enable : (#item Js.t, unit -> unit) Js.meth_callback Js.prop
    method _disable : (#item Js.t, unit -> unit) Js.meth_callback Js.prop
  end

  type t = {
    at_least_one : bool;
    mutable active : item Js.t option;
  }

  let set ?(at_least_one = false) () = {
      at_least_one;
      active = None;
    }

  let enable ~set it =
    (match set.active with
     | None -> ()
     | Some active -> active##disable());
    it##enable();
    set.active <- Some (Js.Unsafe.coerce it :> item Js.t)

  let disable ~set it =
    if set.at_least_one
    then (())
    else
      (it##disable();
       set.active <- None)

  let ctor
      ~(enable : (#item Js.t -> unit -> unit))
      ~(disable : (#item Js.t -> unit -> unit))
      (elt : #item' Js.t) =
    let elt' = (Js.Unsafe.coerce elt :> item' Js.t) in
    let meth = Js.wrap_meth_callback in
    elt'##_enable <- meth enable;
    elt'##_disable <- meth disable;
    (elt' :> item Js.t)
}}

{client{
  module HT = Hashtbl

  let htable =
    HT.create 10

  let set ?at_least_one () =
    let set = set ?at_least_one () in
    HT.add htable (HT.hash set) set;
    set

  let of_server_set set =
    HT.find htable set

  let to_server_set set =
    HT.hash set
}}

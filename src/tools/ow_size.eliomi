(** Size functions for Dom elements. *)

{client{
  (** Convert an int into "%i px". *)
  val int_of_pxstring : Js.js_string Js.t -> int

  (** Extract an int from a string of the form "%i px". *)
  val pxstring_of_int : int -> Js.js_string Js.t

  val get_full_width :
    ?with_width:bool ->
    ?with_padding:bool ->
    ?with_border:bool -> Dom_html.cssStyleDeclaration Js.t -> int

  val get_full_height :
    ?with_height:bool ->
    ?with_padding:bool ->
    ?with_border:bool -> Dom_html.cssStyleDeclaration Js.t -> int

  val width_height : (int * int) React.signal

  val width : int React.signal

  val height : int React.signal

  (** [set_adaptative_width elt f] will make the width of the element
      recomputed using [f] everytime the width of the window changes. *)
  val set_adaptative_width : Dom_html.element Js.t -> (int -> int) -> unit

  (** [set_adaptative_height elt f] will make the width of the element
      recomputed using [f] everytime the height of the window changes. *)
  val set_adaptative_height : Dom_html.element Js.t -> (int -> int) -> unit

  (** Compute the height of an element to the bottom of the page *)
  val height_to_bottom : int -> Dom_html.element Js.t -> int

  val client_top : Dom_html.element Js.t -> int

  val client_bottom : Dom_html.element Js.t -> int

  val client_left : Dom_html.element Js.t -> int

  val client_right : Dom_html.element Js.t -> int
}}

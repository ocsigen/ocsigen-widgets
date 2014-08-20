{client{
  open Html5_types
  open Eliom_content.Html5

  val to_dom_elt : 'a elt -> Dom_html.element Js.t
  val of_dom_elt : Dom_html.element Js.t -> 'a elt

  val to_ul_elt : ul elt -> Dom_html.element Js.t
  val of_ul_elt : Dom_html.element Js.t -> ul elt

  val to_li_elt : li elt -> Dom_html.element Js.t
  val of_li_elt : Dom_html.element Js.t -> li elt
}}

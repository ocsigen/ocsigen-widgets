{client{
  open Eliom_content.Html5

  let to_dom_elt = To_dom.of_element
  let of_dom_elt = Of_dom.of_element

  let to_ul_elt = To_dom.of_ul
  let of_ul_elt = Of_dom.of_uList

  let to_li_elt = To_dom.of_li
  let of_li_elt = Of_dom.of_li
}}

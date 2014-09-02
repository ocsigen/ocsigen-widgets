{client{
  open Html5_types
  open Eliom_content.Html5

}}

{shared{
  open Html5_types

  (* TODO: May need to add some other elements *)
  type any_elt' = [
    | `A of flow5_without_interactive
    | `Abbr
    | `Address
    | `Article
    | `Aside
    | `Audio of flow5_without_media
    | `Audio_interactive of flow5_without_media
    | `B
    | `Bdo
    | `Blockquote
    | `Br
    | `Button
    | `Canvas of flow5
    | `Cite
    | `Code
    | `Command
    | `Datalist
    | `Del of flow5
    | `Details
    | `Dfn
    | `Div
    | `Dl
    | `Em
    | `Embed
    | `Fieldset
    | `Figure
    | `Footer
    | `Form
    | `H1
    | `H2
    | `H3
    | `H4
    | `H5
    | `H6
    | `Header
    | `Hgroup
    | `Hr
    | `I
    | `Iframe
    | `Img
    | `Img_interactive
    | `Input
    | `Ins of flow5
    | `Kbd
    | `Keygen
    | `Label
    | `Map of flow5
    | `Mark
    | `Menu
    | `Meter
    | `Nav
    | `Noscript of flow5_without_noscript
    | `Object of flow5
    | `Object_interactive of flow5
    | `Ol
    | `Output
    | `P
    | `PCDATA
    | `Pre
    | `Progress
    | `Q
    | `Ruby
    | `Samp
    | `Script
    | `Section
    | `Select
    | `Small
    | `Span
    | `Strong
    | `Style
    | `Sub
    | `Sup
    | `Svg
    | `Table
    | `Textarea
    | `Time
    | `U
    | `Ul
    | `Var
    | `Video of flow5_without_media
    | `Video_interactive of flow5_without_media
    | `Wbr
  ]
}}

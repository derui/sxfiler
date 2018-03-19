module C = Sxfiler_common
module R = Jsoo_reactjs

module type Type = sig
  type t

  val to_id: t -> string
end

module type S = sig
  type item
  type item_renderer = item -> bool -> R.React.element Js.t

  class type props = object
    method items : item array Js.readonly_prop
    method itemRenderer : item_renderer Js.readonly_prop
    method selectedIndex : int Js.readonly_prop
  end

  module Component : R.Component.Stateless with type props = props

  val component : (props, unit) R.React.component
end

module Make(T:Type) : S with type item := T.t = struct
  type item_renderer = T.t -> bool -> R.React.element Js.t

  class type props = object
    method items : T.t array Js.readonly_prop
    method itemRenderer : item_renderer Js.readonly_prop
    method selectedIndex : int Js.readonly_prop
  end

  module Component = R.Component.Make_stateless (struct type t = props end)

  let item_container ~key ~body ~selected =
    let class_name = let open Classnames in
      empty
      <|> ("sf-CompletionList_ItemContainer", true)
      <|> ("sf-CompletionList_ItemContainer-selected", selected)
      |> to_string
    in
    R.Dom.of_tag `li
      ~props:(R.element_spec ~key ~class_name ())
      ~children:[|body|]

  let list_container ~renderer ~items ~selected =
    let children = Array.mapi (fun ind item ->
        let selected = ind = selected in
        let key = T.to_id item in
        item_container ~key ~selected ~body:(renderer item selected)
      ) items in
    R.Dom.of_tag `ul ~props:(R.element_spec ~key:"item_list"
                               ~class_name:"sf-CompletionList_ListContainer" ()) ~children

  let component = Component.make (fun props ->
      let class_name = "sf-CompletionList" in
      R.Dom.of_tag `div ~props:(R.element_spec ~class_name ()) ~children:[|
        list_container ~renderer:props##.itemRenderer
          ~items:props##.items
          ~selected:props##.selectedIndex
      |]
    )
end

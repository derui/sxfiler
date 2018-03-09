module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method items: C.Types.File_stat.t array Js.readonly_prop
      method itemRenderer: (C.Types.File_stat.t -> bool -> R.React.element Js.t) Js.readonly_prop
      method selectedIndex: int Js.readonly_prop
    end
  end)

let item_container ~key ~body ~selected =
  let class_name = let open Classnames in
    let open Infix in
    "sf-CompletionList_ItemContainer" <+> ("sf-CompletionList_ItemContainer-selected", selected)
    |> to_string in
  R.Dom.of_tag `li
    ~props:(R.element_spec ~key ~class_name ())
    ~children:[|body|]

let list_container ~renderer ~items ~selected =
  let children = Array.mapi (fun ind item ->
      let selected = ind = selected in
      let key = item.C.Types.File_stat.id in
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

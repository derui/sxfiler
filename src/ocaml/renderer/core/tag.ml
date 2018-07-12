(** The module defines tag to specify store. *)
type 'a def = {
  name: string
}

let def ~name = {name}
let name {name} = name

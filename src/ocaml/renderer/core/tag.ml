type ('a, 'b) def = {
  name: string;
  store: (module Store_intf.S with type t = 'a and type message = 'b);
}

let def ~name ~store = {
  name;
  store;
}

let name {name;_} = name
let store {store;_} = store

module D = Sxfiler_domain
module G = Sxfiler_generated.Configuration

let of_domain (t : D.Configuration_store.t) =
  let list = D.Configuration_store.to_list t in
  List.map
    (fun (key, value) ->
      {
        G.Configuration.key = D.Configuration_store.Key.to_list key;
        json_value = Yojson.Basic.to_string ~std:true (`Assoc [ ("value", value) ]);
      })
    list

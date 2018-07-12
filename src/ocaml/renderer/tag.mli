  (** The type of tag *)
  type 'a def

  (** [def ~name] gets definition of tag. You should specify phantom type for {!def}. *)
  val def: name:string -> 'a def

  (** [name def] gets the name of [def] *)
  val name: 'a def -> string

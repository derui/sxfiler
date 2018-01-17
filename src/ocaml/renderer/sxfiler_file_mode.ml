module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method mode: Js.number Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

module Mode_converter = struct
  type capability = {
    readable: bool;
    writeable: bool;
    executable: bool;
  }
  type t = {
    file: bool;
    directory: bool;
    owner: capability;
    group: capability;
    other: capability
  }

  let bits_to_capability octal =
    {
      readable = octal land 4 = 4;
      writeable = octal land 2 = 2;
      executable = octal land 1 = 1;
    }

  let of_mode_bits mode_bits =
    let bits_per_access = 3 in
    let owner_bits = (mode_bits land 0o700) lsr (bits_per_access * 2)
    and group_bits = (mode_bits land 0o70) lsr bits_per_access
    and other_bits = mode_bits land 0o7
    and file= mode_bits land 0o100000 = 0o100000
    and directory= mode_bits land 0o040000 = 0o040000 in
    {
      file;
      directory;
      owner = bits_to_capability owner_bits;
      group = bits_to_capability group_bits;
      other = bits_to_capability other_bits;
    }

  let capability_to_string {readable;writeable;executable} =
    let readable = if readable then "r" else "-"
    and writeable = if writeable then "w" else "-"
    and executable = if executable then "x" else "-" in
    readable ^ writeable ^ executable

  let to_string t =
    let file_state = match (t.file, t.directory) with
      | (_, true) -> "d"
      | _ -> "-"
    in
    let owner = capability_to_string t.owner
    and group = capability_to_string t.group
    and other = capability_to_string t.other in
    file_state ^ owner ^ group ^ other

end

let component = Component.make (fun props ->
    let mode = Js.float_of_number props##.mode in
    let mode = int_of_float mode in
    R.Dom.of_tag `span
      ~props:(object%js
        val key = Js.Optdef.empty
        val className = Sxfiler_classnames.make ["fp-FileList_FileMode"]
      end)
      ~children:[|
        R.text @@ Mode_converter.(of_mode_bits mode |> to_string)
      |]
  )

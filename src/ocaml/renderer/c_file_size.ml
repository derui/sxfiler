module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type t = object
      method size: Js.number Js.t Js.readonly_prop
    end
  end)

module File_size = struct
  type size_unit =
      Byte
    | KByte
    | MByte
    | GByte
    | TByte
    | Unknown

  type t = {
    size_unit: size_unit;
    aligned_size: float;
    original: float;
  }

  let next_unit = function
    | Byte -> Some KByte
    | KByte -> Some MByte
    | MByte -> Some GByte
    | GByte -> Some TByte
    | TByte -> Some Unknown
    | Unknown -> None

  let size_unit_to_string = function
    | Byte -> "B"
    | KByte -> "K"
    | MByte -> "M"
    | GByte -> "G"
    | TByte -> "T"
    | Unknown -> "-"

  let of_size size =
    let rec calc_unit size current =
      if 0.0 < size && size < 1000.0 then (current, size)
      else begin
        match next_unit current with
        | None -> (current, size)
        | Some next_unit -> calc_unit (size /. 1024.0) next_unit
      end
    in
    let size_unit, aligned_size = calc_unit size Byte in
    {
      size_unit;
      aligned_size;
      original = size;
    }

  let to_string t =
    let size_unit = size_unit_to_string t.size_unit in
    Printf.sprintf "%6.1f%s" t.aligned_size size_unit

end

let component = Component.make (fun props ->
    let size = Js.float_of_number props##.size in
    R.Dom.of_tag `span
      ~props:R.Core.Element_spec.({
          empty with class_name = Some (Classnames.(return "fp-FileItem_FileSize" |> to_string))
        })
      ~children:[|
        File_size.of_size size |> File_size.to_string |> R.text
      |]
  )

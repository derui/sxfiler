module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method size: int64 Js.readonly_prop
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
    original: int64;
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
    let rec calc_unit size current decimal =
      if Int64.(zero <= size) && Int64.(size < 1024L) then (current, Int64.to_float size)
      else begin
        match next_unit current with
        | None -> (current, (Int64.to_float size) +. decimal)
        | Some next_unit -> calc_unit Int64.(div size 1024L) next_unit (Int64.(rem size 1024L |> to_float) /. 1024.0)
      end
    in
    let size_unit, aligned_size = calc_unit size Byte 0.0 in
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
    let size = props##.size in
    R.Dom.of_tag `span
      ~props:R.(element_spec ~class_name:"fp-FileItem_FileSize" ())
      ~children:[
        File_size.of_size size |> File_size.to_string |> R.text
      ]
  )

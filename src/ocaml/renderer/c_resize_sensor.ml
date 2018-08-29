(** A component allow detect resize event of parent component. *)

module R = Jsoo_reactjs

type size =
  { width : int
  ; height : int }

let shrink_key = "shrink"
let expand_key = "expand"
let expand_child_key = "expandChild"

(* styles for sensor element. *)
let style =
  object%js
    val position = Js.string "absolute"

    val left = Js.number_of_float (-10.0)

    val top = Js.number_of_float (-10.0)

    val right = Js.number_of_float 0.0

    val bottom = Js.number_of_float 0.0

    val overflow = Js.string "hidden"

    val zIndex = Js.number_of_float (-1.0)

    val visibility = Js.string "hidden"
  end

let expand_child_style =
  object%js
    val position = Js.string "absolute"

    val left = Js.number_of_float 0.0

    val top = Js.number_of_float 0.0

    val transition = Js.string "0s"
  end

let shrink_child_style =
  object%js
    val position = Js.string "absolute"

    val left = Js.number_of_float 0.0

    val top = Js.number_of_float 0.0

    val transition = Js.string "0s"

    val width = Js.string "200%"

    val height = Js.string "200%"
  end

(* the component to detect shrinking *)
let shrink ~on_scroll ~nodes =
  let child =
    [%e
      div ~key:"shrinkChild"
        ~others:
          (object%js
            val style = shrink_child_style
          end)]
  in
  [%e
    div ~key:"shrink" ~on_scroll
      ~others:
        (object%js
          val style = style
        end)
      ~_ref:(fun e -> R.Ref_table.add nodes ~key:shrink_key ~value:e)
      [child]]

(* the component to detect expanding *)
let expand ~on_scroll ~nodes =
  let child =
    [%e
      div ~key:"expandChild"
        ~_ref:(fun e -> R.Ref_table.add nodes ~key:expand_child_key ~value:e)
        ~others:
          (object%js
            val style = expand_child_style
          end)]
  in
  [%e
    div ~key:"expand" ~on_scroll
      ~others:
        (object%js
          val style = style
        end)
      ~_ref:(fun e -> R.Ref_table.add nodes ~key:expand_key ~value:e)
      [child]]

(** The component to define sensor for parent resizing.
    Component that use this component should define value of [position] as ["relative"].
*)
let t =
  let reset_sensor_elements this =
    let shrink = R.Ref_table.find this##.nodes ~key:shrink_key
    and expand = R.Ref_table.find this##.nodes ~key:expand_key
    and expand_child = R.Ref_table.find this##.nodes ~key:expand_child_key in
    match (shrink, expand, expand_child) with
    | Some shrink, Some expand, Some expand_child ->
      shrink##.scrollLeft := 1000000 ;
      shrink##.scrollTop := 1000000 ;
      expand##.scrollLeft := 1000000 ;
      expand##.scrollTop := 1000000 ;
      expand_child##.style##.width := Js.string "1000000px" ;
      expand_child##.style##.height := Js.string "1000000px"
    | _ -> ()
  in
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method onResized : (size -> unit) Js.readonly_prop

            method getParentSize : (unit -> size) Js.readonly_prop
          end
      end )
    ~spec:
      (R.component_spec
         ~initial_state:(fun _ _ -> object%js end)
         ~initial_custom:(fun this _ ->
             object%js
               val mutable dirty = Js.bool true

               val mutable lastSize = {width = 0; height = 0}

               val mutable currentSize = {width = 0; height = 0}

               val mutable onResized =
                 fun _ ->
                   this##.custom##.rafId := Js.Opt.empty ;
                   if not @@ Js.to_bool this##.custom##.dirty then ()
                   else (
                     this##.custom##.lastSize := this##.custom##.currentSize ;
                     this##.props##.onResized this##.custom##.currentSize )

               val mutable onScroll =
                 fun _ ->
                   let last_size = this##.custom##.lastSize
                   and size = this##.props##.getParentSize () in
                   let dirty = last_size.width <> size.width || last_size.height <> size.height in
                   this##.custom##.dirty := Js.bool dirty ;
                   this##.custom##.currentSize := size ;
                   ( match (dirty, Js.Opt.to_option this##.custom##.rafId) with
                     | true, None ->
                       let cb = Js.wrap_callback this##.custom##.onResized in
                       let id = Dom_html.window##requestAnimationFrame cb in
                       this##.custom##.rafId := Js.Opt.return id
                     | false, None | true, Some _ | false, Some _ -> () ) ;
                   reset_sensor_elements this

               val mutable rafId = Js.Opt.empty
             end )
         ~constructor:(fun this _ -> this##.nodes := Jstable.create ())
         ~component_did_mount:reset_sensor_elements
         (fun this ->
            let on_scroll = this##.custom##.onScroll in
            [%e
              div ~class_name:"global-ResizeSensor"
                ~others:
                  (object%js
                    val style = style
                  end)
                [expand ~on_scroll ~nodes:this##.nodes; shrink ~on_scroll ~nodes:this##.nodes]] ))

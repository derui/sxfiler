(** A component allow detect resize event of parent component. *)

module C = Sxfiler_common
module R = Jsoo_reactjs

type size = {
  width: int;
  height: int;
}

let shrink_key = "shrink"
let expand_key = "expand"
let expand_child_key = "expandChild"

(* styles for sensor element. *)
let style = object%js
  val position = Js.string "absolute"
  val left = Js.number_of_float (-10.0)
  val top = Js.number_of_float (-10.0)
  val right = Js.number_of_float 0.0
  val bottom = Js.number_of_float 0.0
  val overflow = Js.string "hidden"
  val zIndex = Js.number_of_float (-1.0)
  val visibility = Js.string "hidden"
end

let expand_child_style = object%js
  val position = Js.string "absolute"
  val left = Js.number_of_float 0.0
  val top = Js.number_of_float 0.0
  val transition = Js.string "0s"
end

let shrink_child_style = object%js
  val position = Js.string "absolute"
  val left = Js.number_of_float 0.0
  val top = Js.number_of_float 0.0
  val transition = Js.string "0s"
  val width = Js.string "200%"
  val height = Js.string "200%"
end

(* the component to detect shrinking *)
let shrink ~on_scroll ~nodes =
  let props = R.element_spec ()
      ~on_scroll
      ~others:(object%js
        val style = style
      end)
  in
  let children = [
    R.Dom.of_tag `div ~key:"shrinkChild" ~props:(R.element_spec ()
                                                   ~others:(object%js
                                                     val style = shrink_child_style
                                                   end))
  ] in
  R.Dom.of_tag `div ~key:"shrink" ~props ~children
    ~_ref:(fun e -> R.Ref_table.add nodes ~key:shrink_key ~value:e)

(* the component to detect expanding *)
let expand ~on_scroll ~nodes =
  let props = R.element_spec ()
      ~on_scroll
      ~others:(object%js
        val style = style
      end)
  in
  let children = [
    R.Dom.of_tag `div
      ~key:"expandChild"
      ~_ref:(fun e -> R.Ref_table.add nodes ~key:expand_child_key ~value:e)
      ~props:(R.element_spec () ~others:(object%js
                val style = expand_child_style
              end))
  ] in
  R.Dom.of_tag `div ~key:"expand" ~props ~children
    ~_ref:(fun e -> R.Ref_table.add nodes ~key:expand_key ~value:e)


(** The component to define sensor for parent resizing.
    Component that use this component should define value of [position] as ["relative"].
*)
module Component = R.Component.Make_stateful_custom (struct
    class type t = object
      method onResized: (size -> unit) Js.readonly_prop
      method getParentSize: (unit -> size) Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)(struct
    class type t = object
      method dirty: bool Js.t Js.prop
      method lastSize: size Js.prop
      method currentSize: size Js.prop
      method onResized: (float -> unit) Js.prop
      method onScroll: (R.Event.Scroll_event.t -> unit) Js.prop
      method rafId: Dom_html.animation_frame_request_id Js.opt Js.prop
    end
  end)

let component =
  let render this =
    let spec = R.element_spec () ~class_name:"global-ResizeSensor"
        ~others:(object%js
          val style = style
        end)in
    let on_scroll = this##.custom##.onScroll in
    R.Dom.of_tag `div
      ~props:spec
      ~children:[
        expand ~on_scroll ~nodes:this##.nodes;
        shrink ~on_scroll ~nodes:this##.nodes;
      ]
  in
  let reset_sensor_elements this =
    let shrink = R.Ref_table.find this##.nodes ~key:shrink_key
    and expand = R.Ref_table.find this##.nodes ~key:expand_key
    and expand_child = R.Ref_table.find this##.nodes ~key:expand_child_key in
    match (shrink, expand, expand_child) with
    | (Some shrink, Some expand, Some expand_child) -> begin
        shrink##.scrollLeft := 1000000;
        shrink##.scrollTop := 1000000;

        expand##.scrollLeft := 1000000;
        expand##.scrollTop := 1000000;

        expand_child##.style##.width := Js.string "1000000px";
        expand_child##.style##.height := Js.string "1000000px"
      end
    | _ -> ()
  in

  let spec = R.component_spec
      ~constructor:(fun this props ->
          this##.nodes := Jstable.create ();

          this##.custom := object%js
            val mutable dirty = Js.bool true
            val mutable lastSize = {width = 0;height = 0}
            val mutable currentSize = {width = 0;height = 0}
            val mutable onResized = (fun _ ->
                this##.custom##.rafId := Js.Opt.empty;

                if not @@ Js.to_bool this##.custom##.dirty then ()
                else begin
                  this##.custom##.lastSize := this##.custom##.currentSize;
                  this##.props##.onResized this##.custom##.currentSize
                end
              )

            val mutable onScroll = (fun _ ->
                let last_size = this##.custom##.lastSize
                and size = this##.props##.getParentSize () in
                let dirty = last_size.width <> size.width || last_size.height <> size.height in

                this##.custom##.dirty := Js.bool dirty;
                this##.custom##.currentSize := size;

                begin match (dirty, Js.Opt.to_option this##.custom##.rafId) with
                  | (true, None) ->
                    let cb = Js.wrap_callback this##.custom##.onResized in
                    let id = Dom_html.window##requestAnimationFrame cb in
                    this##.custom##.rafId := Js.Opt.return id
                  | (false, None) | (true, Some _) | (false, Some _) -> ()
                end;

                reset_sensor_elements this
              )
            val mutable rafId = Js.Opt.empty
          end
        )
      ~component_did_mount:reset_sensor_elements
      render
  in
  Component.make spec

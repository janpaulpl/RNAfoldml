(* Type of settings for canvas. *)
type settings_t = {
  width : float;
  height : float;
  header_height : float;
  radius_to_width : float;
  pi : float;
  font : string;
  size_title : float;
}

let create_context (filename : string) (s : settings_t) =
  let cr =
    Cairo.create
      (Cairo.PDF.create (filename ^ ".pdf") ~w:s.width ~h:s.height)
  in
  Cairo.select_font_face cr s.font;
  Cairo.set_font_size cr s.size_title;
  Cairo.move_to cr (s.width /. 2.5) 40.;
  cr

let circle_vars set =
  ( set.width /. 2.,
    set.header_height +. (set.width /. 2.),
    set.width *. set.radius_to_width )

let draw_sub_arcs cr set pairs =
  let xc, yc, radius = circle_vars set in
  ignore cr;
  ignore xc;
  ignore yc;
  ignore pairs;
  ignore radius;
  ()

(* [draw_circle cr set] is the incomplete circle outline for
   [circle_graph]*)
let draw_circle cr set =
  let xc, yc, radius = circle_vars set in
  let angle1, angle2 = (set.pi /. 4., 221. *. set.pi /. 180.0) in
  Cairo.set_line_width cr 1.;
  Cairo.Path.sub cr;
  Cairo.arc cr xc yc ~r:radius ~a1:angle1 ~a2:angle2;
  Cairo.stroke cr;

  Cairo.set_source_rgba cr 1. 0.2 0.2 0.6;
  Cairo.set_line_width cr 3.

let settings =
  {
    width = 400.;
    height = 450.;
    header_height = 50.;
    radius_to_width = 1. /. 3.;
    pi = 4. *. atan 500.;
    font = "Sans";
    size_title = 15.;
  }

let circle_graph filename r =
  let name, seq, pairs =
    (Secondary.get_name r, Secondary.get_seq r, Secondary.get_pairs r)
  in
  ignore seq;
  ignore pairs;
  let cr = create_context filename settings in
  draw_circle cr settings;
  draw_sub_arcs cr settings pairs;
  Cairo.show_text cr name;

  (* Cairo.set_font_size cr 14.; Cairo.move_to cr 200. 100.;
     Cairo.show_text cr seq; *)

  (* Cairo.move_to cr 200. 100.; Cairo.show_text cr ; *)
  Cairo.Surface.finish (Cairo.get_target cr)

let compare_graph
    (filename : string)
    (r1 : Secondary.t)
    (r2 : Secondary.t) =
  ignore filename;
  ignore r1;
  ignore r2
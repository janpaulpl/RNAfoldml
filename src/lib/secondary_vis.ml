(* Diagram *)
let circ cr arc =
  let pi = 4. *. atan 500. in
  let (xc, yc) = 128., 128. in
  let radius = 100. in
  let (angle1,angle2) = 45. *. pi /. 180.0, 210. *. pi /. 180.0  in

  Cairo.set_line_width cr 5.;
  arc cr xc yc ~r:radius ~a1:angle1 ~a2:angle2;
  Cairo.stroke cr;

  Cairo.set_source_rgba cr 1. 0.2 0.2 0.6;
  Cairo.set_line_width cr 3.

(* Text *)
let font = try Sys.argv.(1) with _ -> "Sans"

let circle_graph filename r =
  ignore r;
  let surface = Cairo.SVG.create (filename^".svg") ~w:600. ~h:300. in
  let cr = Cairo.create surface in
  circ cr Cairo.arc;
  Cairo.translate cr 200. 0.;
  
  Cairo.select_font_face cr font ~weight:Bold;
  Cairo.set_font_size cr 90.0;
  Cairo.move_to cr 50. 200.;
  Cairo.show_text cr (Secondary.get_name r);

  Cairo.PNG.write surface (filename^".png");
  Cairo.Surface.finish surface


let compare_graph (filename: string) (r1: Secondary.t) (r2: Secondary.t) =
  ignore filename;
  ignore r1;
  ignore r2
let pi = 4. *. atan 500.

let circ cr arc =
  let (xc, yc) = 128., 128. in
  let radius = 100. in
  let angle1 = 45. *. pi /. 180.0 in
  let angle2 = 210. *. pi /. 180.0 in
  Cairo.set_line_width cr 5.;
  arc cr xc yc ~r:radius ~a1:angle1 ~a2:angle2;
  Cairo.stroke cr;

  Cairo.set_source_rgba cr 1. 0.2 0.2 0.6;
  Cairo.set_line_width cr 3.

let circle_graph filename r =
  ignore r;
  let surface = Cairo.SVG.create (filename^".svg") ~w:500. ~h:300. in
  let cr = Cairo.create surface in
  circ cr Cairo.arc;
  Cairo.translate cr 200. 0.;
  Cairo.PNG.write surface (filename^".png");
  Cairo.Surface.finish surface

let compare_graph (filename: string) (r1: Secondary.t) (r2: Secondary.t) =
    ignore filename;
    ignore r1;
    ignore r2
    
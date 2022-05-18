let pi = 4. *. atan 500.

let draw_arc cr arc =
  let xc = 128.
  and yc = 128. in
  let radius = 100. in
  let angle1 = 45. *. pi /. 180.0 in
  let angle2 = 210. *. pi /. 180.0 in
  Cairo.set_line_width cr 5.;
  arc cr xc yc ~r:radius ~a1:angle1 ~a2:angle2;
  Cairo.stroke cr;

  (* draw helping lines *)
  Cairo.set_source_rgba cr 1. 0.2 0.2 0.6;
  Cairo.set_line_width cr 3.

let () =
  let surface = Cairo.SVG.create "vis.svg" ~w:500. ~h:300. in
  let cr = Cairo.create surface in

  (* Arc *)
  draw_arc cr Cairo.arc;

  Cairo.translate cr 200. 0.;

  Cairo.PNG.write surface "vis.png";
  Cairo.Surface.finish surface

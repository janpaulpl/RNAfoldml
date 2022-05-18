(* Canvas *)
let (width, height) = (400., 800.)

(* Diagram *)
let circ cr arc =
  let pi = 4. *. atan 500. in
  let (xc, yc) = width /. 2., height /. 4. in
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
  let name = Secondary.get_name r in 
  (* let seq = Secondary.get_seq r in  *)
  (* let pairs = Secondary.get_pairs r |> Array.to_list |> List.fold_left  in *)

  let cr = Cairo.create(Cairo.PDF.create (filename^".pdf") ~w:width ~h:height) in
  circ cr Cairo.arc;
  
  Cairo.select_font_face cr font;
  Cairo.set_font_size cr 30.;
  Cairo.set_source_rgb cr 139. 0. 0.;
  Cairo.move_to cr (width /. 2.5) 40.;
  Cairo.show_text cr name;
  
  (* Cairo.set_font_size cr 14.;
  Cairo.move_to cr 200. 100.;
  Cairo.show_text cr seq; *)

  (* Cairo.move_to cr 200. 100.;
  Cairo.show_text cr ; *)

  Cairo.Surface.finish(Cairo.get_target cr)

let compare_graph (filename: string) (r1: Secondary.t) (r2: Secondary.t) =
  ignore filename;
  ignore r1;
  ignore r2
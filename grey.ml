(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
    | _ -> wait_key ()
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let level (r,g,b) =
 float_of_int(r)*.0.3 +. float_of_int(g)*.0.59 +. float_of_int(b)*.0.11

let color2grey color = 
  let grey = int_of_float(level color) in (grey, grey,grey)

let image2grey src dst = 
  let (w,h) = get_dims(src) in
  for i = 0 to h do
    for j = 0 to w do
      let color = Sdlvideo.get_pixel_color src i j  in
      Sdlvideo.put_pixel_color dst i j (color2grey(color));
    done
  done

let color2blackwhite color =
  match color with
    (x,y,z) when (x=y)&(x=z) -> if (x <127)then (0,0,0) else (255,255,255)
  | _ -> color

let image2blackwhite src dst =
  let (w,h) = get_dims(src) in
  for i = 0 to h do
    for j = 0 to w do
      let color = Sdlvideo.get_pixel_color src i j  in
      Sdlvideo.put_pixel_color dst i j (color2blackwhite(color));
    done
  done




(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    let new_img = Sdlvideo.create_RGB_surface_format img[] w h in
    image2grey img new_img;
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
      show new_img display;
      (* on attend une touche *)
      wait_key ();
      image2blackwhite new_img new_img;
      show new_img display;
      wait_key (); 
      (* on quitte *)
      exit 0
  end
 
let _ = main ()

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

(*retourne la luminosite moyenne *)
let level (r,g,b) =
 float_of_int(r)*.0.3 +. float_of_int(g)*.0.59 +. float_of_int(b)*.0.11

(*retourne la couleur grise correspondante *)
let color2grey color = 
  let grey = int_of_float(level color) in (grey, grey,grey)

(*applique la couleur grise une image a partir d'une image source *)
let image2grey src dst = 
  let (w,h) = get_dims(src) in
  for i = 0 to h do
    for j = 0 to w do
      let color = Sdlvideo.get_pixel_color src i j  in
      Sdlvideo.put_pixel_color dst i j (color2grey(color));
    done
  done

(*determine le blanc ou le noir en fonction de la couleur*)
let color2blackwhite color =
  match color with
    (x,y,z) when (x=y)&(x=z) -> if (x <127)then (0,0,0) else (255,255,255)
  | _ -> color

(*applique le philtre black&white*)
let image2blackwhite src dst =
  let (w,h) = get_dims(src) in
  for i = 0 to h do
    for j = 0 to w do
      let color = Sdlvideo.get_pixel_color src i j  in
      Sdlvideo.put_pixel_color dst i j (color2blackwhite(color));
    done
  done

(* additionne 9 triplets*)
let add3 (a,b,c)(a1,b1,c1)(a2,b2,c2) (a3,b3,c3) (a4,b4,c4) (a5,b5,c5)(a6,b6,c6) (a7,b7,c7)(a8,b8,c8) = 
  let i = a + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 in
  let j = b + b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 in
  let k = c + c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 in
  (i,j,k)

(* multiplie un triplet par un entier*)
let mult3 (a,b,c) n =
  let i = a*n in
  let j = b*n in
  let k = c*n in
  (i,j,k)

(* divise un triplet par un entier *)
let div3 (a,b,c) n =
  let i = a/n in
  let j = b/n in
  let k = c/n in 
  (i,j,k)

(* calcul la valeur donne par la matrice de convulation*)  
let convulation ((a1,b1,c1),(a2,b2,c2),(a3,b3,c3)) ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) =
  let a = mult3 a1 x1 in
  let b = mult3 b1 y1 in
  let c = mult3 c1 z1 in
  let d = mult3 a2 x2 in
  let e = mult3 b2 y2 in
  let f = mult3 c2 z2 in
  let g = mult3 a3 x3 in
  let h = mult3 b3 y3 in
  let i = mult3 c3 z3 in
  let somme = add3 a b c d e f g h i in 
  div3 somme (x1 + x2 + x3 + y1 + y2 + y3 + z1 + z2 + z3)


let get_matrice  x y img = 
  let a1 = Sdlvideo.get_pixel_color img (x-1) (y-1) in
  let b1 = Sdlvideo.get_pixel_color img (x) (y-1) in
  let c1 = Sdlvideo.get_pixel_color img (x+1) (y-1) in 
  let a2 = Sdlvideo.get_pixel_color img (x-1) (y) in 
  let b2 = Sdlvideo.get_pixel_color img (x) (y) in
  let c2 = Sdlvideo.get_pixel_color img (x+1) (y) in
  let a3 = Sdlvideo.get_pixel_color img (x-1) (y+1) in      
  let b3 = Sdlvideo.get_pixel_color img (x) (y+1) in 
  let c3 = Sdlvideo.get_pixel_color img (x+1) (y+1) in 
  ((a1,b1,c1),(a2,b2,c2),(a3,b3,c3))

let cleaning src dst = 
  let (w,h) = get_dims(src) in
  for i = 1 to h-1 do
    for j = 1 to w-1 do
      let mat = get_matrice i j src in
      let color = convulation mat ((1,1,1),(1,5,1),(1,1,1)) in
      Sdlvideo.put_pixel_color dst i j color;
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
      (*on applique le philtre black&white*)
      image2blackwhite new_img new_img;
      show new_img display;
      wait_key (); 
      (*on applique le philtre de gommage*)
      cleaning new_img new_img;
      show new_img display;
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()

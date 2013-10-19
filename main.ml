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
    Grey.image2grey img new_img;
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
      Grey.image2blackwhite new_img new_img;
      show new_img display;
      wait_key ();
      (*on applique le philtre de gommage*)
      Grey.cleaning new_img new_img;
      show new_img display;
      wait_key ();
      (*on contraste l'image *)
      Grey.contrast new_img new_img;
      show new_img display;
      wait_key ();
      (* on quitte *)
      exit 0
  end

(* la fonction main ne vas pas gerer l'interface*)
let _ = main ()

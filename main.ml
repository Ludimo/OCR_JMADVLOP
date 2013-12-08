(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
   (Sdlvideo.surface_info img).Sdlvideo.h)


    
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

let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    | Sdlevent.KEYDOWN _ -> ()
    | _ -> wait_key ();;

let show img dst =
  let d = Sdlvideo.display_format img in
  begin
        Sdlvideo.blit_surface d dst ();
        Sdlvideo.flip dst
  end;;

let create_surface w h = Sdlvideo.create_RGB_surface
    [ `SWSURFACE ] ~w:w ~h:h ~bpp:32
        ~rmask:Int32.zero ~gmask:Int32.zero ~bmask:Int32.zero ~amask:Int32.zero


let surface2matrix surface =
  let (sizex, sizey) = get_dims surface in
  let matrix = Mmatrix.make sizex sizey false in
        for y = 0 to sizey-1 do
            for x = 0 to sizex-1 do
              let (r,g,b) = Sdlvideo.get_pixel_color surface x y in
                    matrix.(x).(y) <- (r == 0 && g == 0 && b == 0);
            done;
        done;
        matrix


let matrix2surface matrix =
  let width = Mmatrix.get_width matrix
  and height = Mmatrix.get_height matrix in
  let surface = create_surface width height in
    for x = 0 to width - 1 do
        for y = 0 to height - 1 do
            if matrix.(x).(y) then
              begin
                    Sdlvideo.put_pixel_color surface x y Sdlvideo.black
              end
            else
              begin
                    Sdlvideo.put_pixel_color surface x y Sdlvideo.white;
              end
        done;
    done;
    surface


let copy_img src dst =
  let (w,h) = get_dims src in
  for i = 0 to w do
    for j = 0 to h do
      let color = Sdlvideo.get_pixel_color src i j in
      Sdlvideo.put_pixel_color dst i j color
    done
  done
(*---------------------*)

(*affiche les images d'une liste*)
let show_img_list lst =
  let taille = (List.length lst) - 1 in
  for i =0 to taille do
    let (w,h) = get_dims (List.nth lst i) in
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    show (List.nth lst i) display;
    wait_key()
  done


(*--------ROTATION--------------------*)
let load imagepath =
    Sdlvideo.load_BMP imagepath

let save image imagepath =
    Sdlvideo.save_BMP image imagepath


let resize imagepath w h =
  begin
    ignore(Sys.command ("convert " ^ imagepath ^ " -resize "
    ^ string_of_int (int_of_float (float w /. float h *. 300.)) ^ "x300" ^
    " lab/small.png"));
    let small = Sdlloader.load_image "lab/small.png" in
    let (w, h) = get_dims small in
    let small_grey = create_surface w h in
    Grey.image2grey small small_grey;
    let small_bin = create_surface w h in
    Grey.image2blackwhite small_grey small_bin;
    small_bin
    end

let rotate imagepath =
  let surface = load imagepath in
  let (w,h) = get_dims surface in
  let small_surface = resize imagepath w h in
  let bin_matrix = surface2matrix surface in
  let bin_matrix_small = surface2matrix small_surface in
  let angle = Rotation.find_angle bin_matrix_small (-.10.0) 10.0 1.0 in
  let angle =
        Rotation.find_angle bin_matrix_small (angle -. 0.5) (angle +. 0.5) 0.1
  in
  let rotated = Rotation.rotate bin_matrix (angle) in
  let out = matrix2surface rotated in
        save out "lab/temp.bmp"

(*------------------*)


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
    let rot_img = Sdlvideo.create_RGB_surface_format img[] w h in
    copy_img img rot_img;
    let rot_img1 = Sdlvideo.create_RGB_surface_format img[] (max w h) (max w h)
    in
    copy_img img rot_img1;
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
      Grey.image2blackwhite new_img img;
      show img display;
      wait_key ();
      (*on applique le philtre de gommage*)
      Grey.cleaning img new_img;
      show new_img display;
      wait_key ();
      (*on contraste l'image *)
      Grey.contrast new_img img;
      show img display;
      wait_key ();
      (*on rebinarise*)
      Grey.image2blackwhite img new_img;
      show new_img display;
      wait_key ();
     (*rotate "test2.bmp";*)
     (* Line.print_list (Line.decoupe_line1 new_img);
      print_string "\n";
      Line.print_list (Line.decoupe_line2 new_img);
      print_string "\n";*)
      Line.print_list_couple (Line.list_line_zone img);
      print_string "\n";
      print_string "\n";
      (*Line.print_list_couple (Line.get_char_cid img);
      print_string "\n";*)
      (*Pretrait.trait img new_img ;*)
      (*on determine les zones de texte prochainement*)
      (* Line.write_green new_img img;
         show img display;
         wait_key ();*)
      (*teste de rotation*)
      (* on quitte *)
      exit 0
  end

(* la fonction main ne vas pas gerer l'interface pour l'instant*)
let _ = main ()

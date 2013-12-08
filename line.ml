(* ---------FONCTIONS BASIQUES ------------- *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
    

let is_pixel_black img x y =
  (Sdlvideo.get_pixel_color img x y) = (0,0,0)

let pixel_is_green img x y =
  (Sdlvideo.get_pixel_color img x y) = (0,255,0)
  
let line_is_green img i =
  pixel_is_green img i 1

    
(*----------LINE-----------*)

let pixel_is_white img x y = 
  (Sdlvideo.get_pixel_color img x y) = (255,255,255)
    
let is_white_and_black (a,b) =
  match (a,b) with
  |(true,false) -> true;
  |_ -> false

let is_black_and_white (a,b) =
  match (a,b) with
  |(false,true) -> true;
  |_ -> false
    
let line_is_white img y w  =
  let a = ref true in
  for i = 0 to w do
    a := !a & (pixel_is_white img i y)
  done ;
  !a


let copy_line src dst h w =
  for i = 0 to w do
    let color = Sdlvideo.get_pixel_color src i h in
    Sdlvideo.put_pixel_color dst i h color
  done
    
let green_line dst h w =
  for i = 0 to w do
    Sdlvideo.put_pixel_color dst i h (0,255,0)
  done
    
    
let write_green_up src dst =
  let (w,h) = get_dims(src) in
  for j = 1 to h-1 do
    if is_white_and_black ((line_is_white src (j-1) w),(line_is_white src j w)) then
      begin
        green_line dst (j-1) w;
        copy_line src dst j w;
      end
    else
     copy_line src dst j w
  done
    
let write_green_down src dst =
  let (w,h) = get_dims(src) in
  for j = 1 to h-1 do
    if is_black_and_white  ((line_is_white src j w),(line_is_white src (j+1) w)) then
      begin
        green_line dst (j+1) w;
        copy_line src dst j w;
      end
    else
      copy_line src dst j w
  done
(* -------------END-LINE-----------------*)
let  rec print_list lst =
  match lst with 
  |[]-> ();
  |a::b ->
    begin 
      print_string (string_of_int a);
      print_string ",";
      print_list b
    end

let print_couple (a,b) =
  let str_a = string_of_int a in
  let str_b = string_of_int b in
  print_string "(";
  print_string str_a;
  print_string ",";
  print_string str_b;
  print_string ")";
  print_string ";"

let rec print_list_couple lst =
  match lst with 
  |[]-> ();
  |a::b ->
    begin 
      print_couple a;
      print_string ",";
      print_list_couple b
    end


let rec conc_list l1 l2 =
  match (l1,l2) with
  |([],[]) -> []
  |([],l) -> []
  |(l,[]) ->[]
  |(a::b,x::y) -> (a,x)::(conc_list b y)
 
let decoupe_line1 img =
  let (w,h) = get_dims(img) in
  let list = ref [] in
  for j = 1 to h-1 do
    if is_white_and_black ((line_is_white img (j-1) w),(line_is_white img j w)) then
      list := (j)::(!list)
  done;
  (List.rev !list)
    
let decoupe_line2 img =
  let (w,h) = get_dims(img) in
  let list = ref [] in
  for j = 1 to h-1 do
    if is_black_and_white (((line_is_white img j) w),(line_is_white img (j+1) w)) then
      list := (j)::(!list)
  done;
  (List.rev !list)


let list_line_zone img =
  (conc_list (decoupe_line1 img) (decoupe_line2 img))


(*--------------COLUMN----------------*)

let is_column_white img x start_line end_line =
  let a = ref true in
  for j = start_line to end_line do
    a := !a && (pixel_is_white img x j)
  done ;
  !a


let copy_column src dst w start_line end_line =
  for i = start_line to end_line do
    let color = Sdlvideo.get_pixel_color src w i in
    Sdlvideo.put_pixel_color dst w i color
  done

let green_column img w start_line end_line =
  for i = start_line to end_line do
    Sdlvideo.put_pixel_color img w i (0,255,0)
  done

(*obtenir les coordonnees du coin superieur gauche des caracteres*)
let get_char_csg img =
  let (w,h)= get_dims img in
  let list = ref [] in
  let lst_up = decoupe_line1 img in
  let zone = list_line_zone img in
  let l = (List.length zone) - 1 in
  for i = 0  to l do
    let (start_line, end_line) = List.nth zone i in
    for j = 1 to w do
      if is_white_and_black ((is_column_white img (j-1) start_line end_line),(is_column_white img j start_line end_line)) then
         list := (j, (List.nth lst_up i))::(!list)
    done
  done;
  (List.rev !list)

(*obtenir les coordonnees du coin inferieur droit des caracteres*)
let get_char_cid img =
  let (w,h)= get_dims img in
  let list = ref [] in
  let lst_down = decoupe_line2 img in
  let zone = list_line_zone img in
  let l = (List.length zone) - 1 in
  for i = 0  to l do
    let (start_line, end_line) = List.nth zone i in
    for j = 1 to w do
      if is_black_and_white ((is_column_white img (j) start_line end_line),(is_column_white img (j+1) start_line end_line)) then
         list := (j, (List.nth lst_down i))::(!list)
    done
  done;
  (List.rev !list)
(*Liste qui renvoit la position des caracteres selon les coodonees des coins superieurs et inferieurs*)
let get_char_coord img =
  conc_list (get_char_csg img) (get_char_cid img)


let write_green src dst =
  write_green_up src dst;
  write_green_down dst dst



(*---------------------DECOUPE---------------------- *)
  

let copy_img src dst start_x start_y end_x end_y =
  for i = start_x to end_x do
    for j = start_y to end_y do
      let color = Sdlvideo.get_pixel_color src i j in
      Sdlvideo.put_pixel_color dst i j color
    done
  done



(*
let show_nth_img list nth img =
  let (w,h) = get_dims img in
  let (start_line, end_line) = List.nth list nth in
  let new_img = Sdlvideo.create_RGB_surface_format img[] w (end_line - start_line + 1) in
    copy_img img new_img start_line end_line;
  let display = Sdlvideo.set_video_mode w (end_line - start_line +1) [`DOUBLEBUF] in
  show new_img display;
  *)

let get_decoupage img =
  let list_decoupe = get_char_coord img in
  let list_img = ref [] in
  for i = 0 to (List.length list_decoupe)-1 do
    let (a,b) = List.nth list_decoupe i in
    let (start_x,start_y) = a in
    let (end_x,end_y) = b in
    let new_img = Sdlvideo.create_RGB_surface_format img[] (end_x-start_x+1)(end_y-start_y+1) in
    copy_img img new_img start_x start_y end_x end_y;
    list_img := new_img:: !list_img
  done;
  List.rev !list_img

(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
    
let sup3 (a,b,c) (x,y,z) =
  let s1 = (a + b + c) in
  let s2 = (x + y + z) in
  s1 >= s2
    
(*----------LINE-----------*)

let pixel_is_white img x y = 
  sup3 (Sdlvideo.get_pixel_color img x y) (0,255,0)
    
let is_white_and_black (a,b) =
  match (a,b) with
  |(true,false) -> false;
  |_ -> true

let is_black_and_white (a,b) =
  match (a,b) with
  |(false,true) -> false;
  |_ -> true
    
let line_is_white img y w  =
  let a = ref true in
  for i = 0 to w do
    a := !a && (pixel_is_white img i y)
  done ;
  !a
    
let line_white img y w =
  line_is_white img y w
    
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
      copy_line src dst j w
    else
      begin
        green_line dst (j-1) w;
        copy_line src dst j w;
      end
  done
    
let write_green_down src dst =
  let (w,h) = get_dims(src) in
  for j = 1 to h-1 do
    if is_black_and_white  ((line_is_white src j w),(line_is_white src (j+1) w)) then
      copy_line src dst j w
    else
      begin
        green_line dst (j+1) w;
        copy_line src dst j w;
      end
  done
(* -------------END-LINE-----------------*)


let same_pixel (a,b,c) (x,y,z) =
  (a == x)&&(b == y) && (c == z)

let pixel_is_green img x y =
  same_pixel (Sdlvideo.get_pixel_color img x y) (0,255,0)
  
let line_is_green img i =
  pixel_is_green img i 1

let rec conc_list l1 l2 =
  match (l1,l2) with
  |([],[]) -> []
  |([],l) -> []
  |(l,[]) ->[]
  |(a::b,x::y) -> (a,x)::(conc_list b y)
 

let digit n =
  let rec aux a = function
    | 0 -> a
    | x -> aux (x mod 10 :: a) (x/10)
  in
  aux [] n



let decoupe_line1 img =
  let (w,h) = get_dims(img) in
  let list = ref [] in
  for j = 1 to h-1 do
    if is_white_and_black ((line_is_white img (j-1) w),(line_is_white img j w)) then
      ()
    else
      list := (j)::(!list)
  done;
  (List.rev !list)
    
let decoupe_line2 img =
  let (w,h) = get_dims(img) in
  let list = ref [] in
  for j = 1 to h-1 do
    if is_white_and_black (((line_is_white img j) w),(line_is_white img (j+1) w)) then
      ()
    else
      list := (j)::(!list)
  done;
  (List.rev !list)

let rec correct_list l1 =
  match l1 with
  |[] -> []
  |a::[] -> []
  |(a,b)::(c,d)::e -> (b,c)::(correct_list e)


let list_line_zone img =
  (conc_list (decoupe_line1 img) (decoupe_line2 img))

let list_line_zone1 img =
  (correct_list (list_line_zone img)) 

let print_couple (a,b) =
  let str_a = string_of_int a in
  let str_b = string_of_int b in
  print_string "(";
  print_string str_a;
  print_string ",";
  print_string str_b;
  print_string ")";
  print_string ";"
  
  


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

let write_green_left src dst =
  let (w,h)= get_dims src in
  let zone = list_line_zone1 src in
  let l = (List.length zone) - 1 in
  for i = 0  to l do
    let (start_line, end_line) = List.nth zone i in
    print_couple (start_line, end_line);
    for j = 1 to w do
      if is_white_and_black ((is_column_white src (j-1) start_line end_line),(is_column_white src i start_line end_line)) then
        copy_column src dst j start_line end_line
      else
        begin
          green_column dst (j-1) start_line end_line;
          copy_column src dst j start_line end_line;
        end
    done
  done



let write_green src dst =
  write_green_up src dst;
  write_green_down dst dst

let write_green_char src dst =
  write_green_left src dst



(*---------------------DECOUPE---------------------- *)

let copy_img src dst start_line end_line =
  let (w,h) = get_dims src in
  for i = 0 to w do
    for j = start_line to end_line do
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
  let list_decoupe = list_line_zone1 img in
  let list_img = ref [] in
  let (w,h)= get_dims img in
  for i = 0 to (List.length list_decoupe)-1 do
    let (start_line, end_line) = List.nth list_decoupe i in
    let new_img = Sdlvideo.create_RGB_surface_format img[] w (end_line - start_line + 1) in
    copy_img img new_img start_line end_line;
    list_img := new_img:: !list_img
  done;
  List.rev !list_img

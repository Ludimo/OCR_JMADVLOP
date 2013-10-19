(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
    
let sup3 (a,b,c) (x,y,z) =
  let s1 = (a + b + c)/3 in
  let s2 = (x + y + z)/3 in
  s1 >= s2
    
let pixel_is_white img x y = 
  sup3 (Sdlvideo.get_pixel_color img x y) (127,127,127)
    
let et a b =
  a&&b
    
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
    if (line_is_white src (j-1) w) && (line_is_white src j w) then
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
    if (line_is_white src (j+1) w) && (line_is_white src j w) then
      copy_line src dst j w
    else
      begin
	green_line dst (j+1) w;
	copy_line src dst j w;
      end
  done
  

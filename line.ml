(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let sup3 (a,b,c) (x,y,z) =
  let s1 = (a + b + c)/3 in
  let s2 = (x + y + z)/3 in
  s1 >= s2

let pixel_is_white img x y = 
  sup3 (Sdlvideo.get_pixel_color img x y) (127,127,127)

let line_is_white img y w white =
    for i = 0 to w do
      while !white do
	white := !white && (pixel_is_white img i y)
      done
    done

let line_white img y w =
  let white = ref true in 
  line_is_white img y w white;
  !white

let copy_line src dst h w =
  for i = 0 to w do
    let color = Sdlvideo.get_pixel_color src i h in
    Sdlvideo.put_pixel_color dst i h color
  done

let green_line dst h w =
  for i = 0 to w do
    Sdlvideo.put_pixel_color dst i h (0,255,0)
 done


let write_green src dst =
  let (w,h) = get_dims(src) in
  for j = 1 to h-1 do
    if (line_white src (j-1) w) && (line_white src j w) then
      copy_line src dst j w
    else
      green_line dst j w
  done



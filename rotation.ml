let get_dims img =
  (float_of_int((Sdlvideo.surface_info img).Sdlvideo.w),float_of_int((Sdlvideo.surface_info img).Sdlvideo.h))


let get_rot i j img ang=
  let (x,y) = get_dims img in
  let (xrot,yrot) = ((x/.2.)+.(i-.x/.2.)*.(cos(ang))-.(j-.y/.2.)*.(sin(ang)),(y/.2.)+.(i-.x/.2.)*.(sin(ang))+.(j-.y/.2.)*.(cos(ang)))in
  (int_of_float(xrot),int_of_float(yrot))



let rotate src dst ang =
  let (x,y) = get_dims src in
  for i = 0 to int_of_float(x) do
    for j = 0 to int_of_float(y) do
      let (xrot, yrot) = get_rot (float_of_int(i)) (float_of_int(j)) src ang in
      let color = Sdlvideo.get_pixel_color src xrot yrot in
      Sdlvideo.put_pixel_color dst i j color
    done
  done

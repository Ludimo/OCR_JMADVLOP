let get_dims img =
  (((Sdlvideo.surface_info img).Sdlvideo.w),
   (Sdlvideo.surface_info img).Sdlvideo.h)


let get_rot l m img ang=
  let(x1,y1) = get_dims img in
  let(i,j) = (float_of_int(l),float_of_int(m))in
  let (x,y) = (float_of_int (x1),float_of_int (y1)) in
  let (xrot,yrot) = ((x/.2.)+.(i-.x/.2.)*.(cos(ang))-.(j-.y/.2.)*.(sin(ang)),
                     (y/.2.)+.(i-.x/.2.)*.(sin(ang))+.(j-.y/.2.)*.(cos(ang)))in
  (int_of_float(xrot),int_of_float(yrot))



let rotate src dst ang =
  let (x,y) = get_dims src in
  for i = 0 to (x) do
    for j = 0 to (y) do
      let (xrot, yrot) = get_rot i j src ang in
      let color = Sdlvideo.get_pixel_color src xrot yrot in
      Sdlvideo.put_pixel_color dst i j color
    done
  done

let sup3 (a,b,c) (x,y,z) =
  let s1 = (a + b + c)/3 in
  let s2 = (x + y + z)/3 in
  s1 <= s2
    
let pixel_is_black img x y = 
  sup3 (Sdlvideo.get_pixel_color img x y) (0,255,0)



let deg_to_rad rad = 
  let pi = 4.0 *. atan 1.0 in 
      rad*.180./.pi

let rad_to_deg deg =
  let pi = 4.0 *. atan 1.0 in
  deg*.pi/.180.

let get_tab_max tab size_x size_y =
  let r = ref (0) in
  begin
    for i = 0 to size_x do
      for j = 0 to size_y do
        if (tab.(i).(j) > !r) then
          r:= tab.(i).(j)
      done
    done;
      !r
  end
    
let get_tab_ang tab size_x size_y e =
  let r = ref 0 in  
  for i = 0 to size_x do
    for j = 0 to size_y do
      if (tab.(i).(j)) == e then
        r:= i
    done
  done;
  !r


let nbr_pixel_black img j =
  let (x,y) = get_dims img in
  let r = ref 0 in 
  for i = 0 to x do
    if pixel_is_black img i j then
      r := !r +1
  done;
  !r

let parcours img =
  let (w,h) = get_dims img in
  for j = 0 to h do
    let po = nbr_pixel_black img j in
    print_string "po";
    print_string (string_of_int po);
    print_string "\n"
  done
    

let detect img =
  let (x,y) = get_dims img in
  let max = max x y in
  let (size_x,size_y) = (180,5*max)in
  let tab = Array.make_matrix size_x size_y 0 in
  for i = 0 to (x) do
    print_string "i";
    print_string (string_of_int i);
    print_string "\n";
    for j = 0 to (y) do
      if (pixel_is_black img i j) then
        for k = 0 to size_x -1 do
          print_string "k";
          print_string (string_of_int k);
          print_string "\n"; 
          let r = (float_of_int i)*.cos(rad_to_deg(float_of_int(k))) +.
            (float_of_int j)*.sin(rad_to_deg(float_of_int(k))) 
            +. float_of_int(max) in
          print_string "r";
          print_string (string_of_int (int_of_float(r)));
          print_string "\n"; 
          tab.(k).(int_of_float(r)) <- tab.(k).(int_of_float(r)) + 1
        done 
    done
  done;
    (get_tab_ang tab size_x size_y (get_tab_max tab size_x size_y)) - 90

let rotati src dst = 
  rotate src dst (rad_to_deg (float_of_int(detect src)))

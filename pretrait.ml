let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
   (Sdlvideo.surface_info img).Sdlvideo.h)

let trait src dst =
  let (w,h) = get_dims src in
  let new_img = Sdlvideo.create_RGB_surface_format src[] w h in
  Grey.image2grey src new_img;
  Grey.image2grey new_img src;
  Grey.cleaning src new_img;
  Grey.contrast new_img src;
  Grey.image2blackwhite src dst;

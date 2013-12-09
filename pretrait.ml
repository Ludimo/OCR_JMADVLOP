let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
   (Sdlvideo.surface_info img).Sdlvideo.h)

let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end



let load imagepath =
    Sdlvideo.load_BMP imagepath

let save image imagepath =
    Sdlvideo.save_BMP image imagepath

let trait imagepath =
  let src = load imagepath in
  let (w,h) = get_dims src in
  let new_img = Sdlvideo.create_RGB_surface_format src[] w h in
  Grey.image2grey src new_img;
  Grey.image2grey new_img src;
  Grey.cleaning src new_img;
  Grey.contrast new_img src;
  Grey.image2blackwhite src new_img;
  save new_img "trait.bmp"

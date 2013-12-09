let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlttf.init ();
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let _ = GMain.init ()
  
(*Fenêtre principale*)
let window = GWindow.window
  ~width:1200
  ~height:800
  ~resizable:false
  ~title:"OCR Polvdamaj" ()

(*Conteneur principal, pour pouvoir insérer plusieurs widget*)
let columnbox = GPack.vbox
  ~homogeneous:false
  ~spacing:10
  ~border_width:10
  ~packing:window#add ()

(*Conteneur secondaire pour l'image et la zone de texte*)
let hbox = GPack.hbox
  ~homogeneous:false
  ~spacing:10
  ~border_width:10
  ~packing:columnbox#add ()


(*Conteneur boutons*)
let bbox = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~packing:columnbox#add ()


let scroll = GBin.scrolled_window
    ~hpolicy:`ALWAYS
    ~vpolicy:`ALWAYS
    ~packing: hbox#add ()


let textView = GText.view
  ~border_width:10
  ~width:100
  ~height:600
  ~packing:hbox#add ()




(*Image*)
let image_path = ref ""
let image = GMisc.image
     ~file:(!image_path)
     ~packing:scroll#add_with_viewport ()

let update_image str =
    image#set_file str

let may_open btn () = Gaux.may update_image btn#filename

(*Bouton Ouvrir image*)
let openButton =
  let button = GFile.chooser_button
    ~action: `OPEN
    ~packing:bbox#add () in
  ignore (button#connect#selection_changed ~callback:(may_open button));
  button

(* Bouton Pretraitement *)
let processonly funct imagepath =
    funct imagepath


let pretraitement () =  processonly (Pretrait.trait) ("test2.bmp")

let pretraitement_button = 
  let button = GButton.button
    ~label:"Pretraitement"
    ~packing:bbox#add () in
  ignore(button#connect#clicked ~callback:(pretraitement));
  button

(* Bouton save *)
let save (text : GText.view) file =
  let och = open_out file in
            output_string och (text#buffer#get_text ());
            close_out och

let saver (button : GFile.chooser_button) () =
  match button#filename with
  |Some i -> save textView i
  |_ -> ()

let saveButton =
  let button = GFile.chooser_button
    ~action:`OPEN
    ~packing:bbox#add () in
  ignore (button#connect#selection_changed ~callback:(saver button));
button



(*Bouton Quitter*)
let quit =
  let button = GButton.button
    ~stock:`QUIT
    ~packing:bbox#add () in
  ignore (button#connect#clicked ~callback:(GMain.quit));
  button

let _ =
  sdl_init();
  ignore (window#connect#destroy ~callback:GMain.quit);
  window#show ();
  GMain.main ()

# TP sdl

OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlttf.cmxa
OCAMLGTK=  -I +sdl -I +site-lib/sdl -I +site-lib/lablgtk2 lablgtk.cmxa gtkInit.cmx

LIBS = lablgtk2.cmxa


main: main.ml
	 ${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o main grey.ml line.ml rotation.ml pretrait.ml main.ml

gui: gui.ml
	${OCAML} ${OCAMLGTK} ${OCAMLLD} -o gui grey.ml line.ml rotation.ml pretrait.ml gui.ml


clean::
	rm -f *~ *.o *.cm? gui main

# FIN

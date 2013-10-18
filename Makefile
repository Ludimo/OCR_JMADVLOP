# TP sdl
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
grey: grey.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o grey grey.ml
 
clean::
	rm -f *~ *.o *.cm? grey
 
# FIN

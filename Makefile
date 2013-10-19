# TP sdl

OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa

main: main.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o main grey.ml main.ml

clean::
	rm -f *~ *.o *.cm? main

# FIN

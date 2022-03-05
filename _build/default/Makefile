build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc

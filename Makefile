.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f battlefield.zip
	zip -r battlefield.zip . -x@exclude.lst

clean:
	dune clean
	rm -f battlefield.zip

doc:
	dune build @doc

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

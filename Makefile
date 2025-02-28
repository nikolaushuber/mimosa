.PHONY: docker
docker:
	docker pull ocaml/opam:debian-11-ocaml-5.2
	docker run -it -v ./:/home/opam/mimosa ocaml/opam:debian-11-ocaml-5.2

.PHONY: install
install:
	opam install ./mimosa --yes

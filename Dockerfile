FROM ocaml/opam:debian-11-ocaml-5.2

WORKDIR /home/opam

USER root
COPY . .

RUN opam install . --yes

FROM ocaml/opam:debian-11-ocaml-5.2

# copy mimosa sources
WORKDIR /home/opam/mimosa
COPY . .


# build and install mimosa
RUN opam install . --yes

# remove mimosa source code again
WORKDIR /home/opam/
RUN rm -rf ./mimosa

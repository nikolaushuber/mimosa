# Installation

In order to use Mimosa you need to install a compatible OCaml environment. The compiler and simulator can then be installed via [dune](https://dune.build):

```text
git clone https://github.com/nikolaushuber/mimosa.git
cd mimosa
dune build
dune install
```

## Docker

You can also run Mimosa inside a docker container. The easiest option is to use one of the provided [ocaml/opam](https://hub.docker.com/r/ocaml/opam) images:

```text
git clone https://github.com/nikolaushuber/mimosa.git
cd mimosa
docker run -it -v ./:/home/opam/mimosa/ ocaml/opam:debian-11-ocaml-5.2
```

The `docker run` command creates a container from the provided image (the tag behind the `ocaml/opam` image name indicates that we would like to use version 5.2 of the OCaml compiler running on-top of a Debian 11, this image is automatically downloaded if it is not present on your computer). We also instruct docker that we would like to interact with the container after it has booted (`-it`), and that it should mirror the current directory inside the container at `/home/opam/mimosa/`.

Once the container is up and running you can install mimosa inside it:

```text
cd mimosa
opam install . --yes
```

# Installation

First, you need to download/clone the Mimosa repository:

```text
git clone https://github.com/nikolaushuber/mimosa.git
cd mimosa
```

In order to use Mimosa you need to install a compatible OCaml environment. The compiler and simulator can then either be installed via [dune](https://dune.build):

```text
dune build
dune install
```

or via [opam](https://opam.ocaml.org):

```text
opam install . --yes
```

## Docker

You can also run Mimosa inside a docker container. The easiest option is to use one of the official [ocaml/opam](https://hub.docker.com/r/ocaml/opam) images:

```text
docker pull ocaml/opam:debian-11-ocaml-5.2
docker run -it -v ./:/home/opam/mimosa/ --name mimosa ocaml/opam:debian-11-ocaml-5.2
```

The `docker run` command creates a container from the provided image (the tag behind the `ocaml/opam` image name indicates that we would like to use version 5.2 of the OCaml compiler running on-top of Debian 11). We also instruct docker that we would like to interact with the container after it has booted (`-it`), and that it should mirror the current directory inside the container at `/home/opam/mimosa/`. The `--name mimosa` flag assigns the name `mimosa` to the container, so that we can easily restart it later.

Once the container is up and running you can install mimosa inside it:

```text
cd mimosa
opam install . --yes
```

You can leave the container at any point via the `exit` command. Since we named it during creation, we can later restart and reattach to it:

```text
docker start mimosa
docker attach mimosa
```

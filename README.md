# The Mimosa Language

This repository contains tools for working with Mimosa, a programming language specifically made for embedded (control) applications. It builds upon the [MIMOS model]((https://doi.org/10.1007/978-3-031-08143-9_2)) of computation.

## Documentation

For further information on the Mimosa language, please refer to the documentation:

[nikolaushuber.github.io/mimosa/](https://nikolaushuber.github.io/mimosa/)

## Installation

### Downlaod this repository

In order to install Mimosa, you have to download/clone this repository:

```
git clone https://github.com/nikolaushuber/mimosa.git
cd mimosa
```

### Setup OCaml

Mimosa is implemented in OCaml, therefore, an OCaml development environment is needed (including [opam](https://opam.ocaml.org), [dune](https://dune.build), and a recent OCaml compiler). There are various ways of installing OCaml, please refer to the official [documentation](https://www.ocaml.org).

It is also possible to use a docker image, which already comes with an OCaml development environment installed. If you want to install Mimosa without docker please jump to [the next section](#install-mimosa).

```
docker pull ocaml/opam:debian-11-ocaml-5.2
docker run -it -v ./:/home/opam/mimosa --name mimosa ocaml/opam:debian-11-ocaml-5.2
```

This will pull an image from DockerHub which already contains an OCaml 5.2 compiler.
The second command will run a container from this image, will mirror the current folder inside the container at `/home/opam/mimosa`, and use the name `mimosa` to refer to the container.

This should leave you at `/home/opam` inside the container, so you can switch to the mirrored directory with

```
cd mimosa
```

You can leave the container at any time with `exit`. To continue, you can start and attach to the container again like so:

```
docker start mimosa
docker attach mimosa
```

### Install Mimosa

The easiest way of installing Mimosa is through the OCaml package manager [opam](https://opam.ocaml.org):

```
opam install . --yes
```

Alternatively, it can be installed with dune:

```
dune build
dune install
```

## Running tests

To run all tests in this repository you can run

```
dune runtest
```

If there is no output, all tests ran successfully.

## Examples

The `/examples` folder holds various examples of Mimosa, and how to simulate programs written in it. Please refer to the README inside the folder for more information.

## License

MIT

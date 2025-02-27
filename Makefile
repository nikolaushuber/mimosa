.PHONY: default
default:
	@echo "Please select one of the available targets (install, test, clean)"

.PHONY: install
install:
	opam exec -- opam install . --yes

.PHONY: test
test:
	opam exec -- dune runtest

.PHONY: clean
clean:
	rm -rf _opam
	rm -rf _build

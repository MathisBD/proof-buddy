# Description

This is a toy implementation of a proof assistant (similar in spirit to Coq or Lean). It is not intented to serve as an example implementation for other people, but rather as a learning experience for myself.

# Initial setup

1. Create a local OPAM switch and install required dependencies :
```
opam switch create . 5.1.1
eval $(opam env)
```

2. Install optional (development) dependencies. This depends on your editor, for VS Code use :
```
opam install ocaml-lsp-server ocamlformat user-setup
opam user-setup install
```

# Building and running

Use the commands in the Makefile such as `make`, `make test` and `make clean`.


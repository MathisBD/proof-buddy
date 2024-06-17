# Description

This is a toy implementation of a proof assistant (similar in spirit to Coq or Lean). It is not intented to serve as an example implementation for other people, but rather as a learning experience for myself.

# Initial setup

1. Create a local OPAM switch :
```
opam switch create . 5.1.1
```

2. Install required dependencies : 
```
opam install . --deps-only
```

3. Install optional (development) dependencies. For VS Code :
```
opam install ocaml-lsp-server ocamlformat user-setup
opam user-setup install
```

# Building and running

To build : 
```
dune build
```


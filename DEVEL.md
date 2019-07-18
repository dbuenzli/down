# Testing toplevel

Add an empty `.ocamlinit` file at the root of the distribution to disable
`~/.ocamlinit`. Then it's just a mater of:

```
touch .ocamlinit
brzo ocaml --top 
```

# Generating TTY width data

```
ocamlbuild -use-ocamlfind tty_width/gen.native
./gen.native -t # test and get info about map
./gen.native > src/down_tty_width.ml
```

Test the module:

```
ocamlbuild -use-ocamlfind tty_width/test.native
./test.native
```



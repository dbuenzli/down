# Testing toplevel

Add an empty `.ocamlinit` file at the root of the distribution to
disable your own `~/.ocamlinit`:

```
echo '#directory "_build/src";; #load "down.cma"' > .ocamlinit
topkg build 
ocaml
```

or

```
touch .ocamlinit
brzo ocaml --top 
```

# Generating TTY width data

```
b0 -a tty-width-gen -- -t  # test and get info about map
b0 -a tty-width-gen -- > src/down_tty_width.ml
```

Test the module:

```
b0 -a tty-width-test
```



open B0_kit.V000
open Result.Syntax

(* OCaml libraries *)

let down = B0_ocaml.libname "down"
let uucp = B0_ocaml.libname "uucp"
let unix = B0_ocaml.libname "unix"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

(* Down libraries *)

let down_lib =
  let doc = "An OCaml toplevel upgrade" in
  let requires = [compiler_libs_toplevel] in
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib down ~doc ~requires ~srcs

(* TTY width data generation *)

let tty_width_gen =
  let doc = "Generate OCaml source for TTY width data" in
  let requires = [unix; uucp] in
  let srcs = [ `File ~/"tty_width/gen.ml"] in
  B0_ocaml.exe "tty-width-gen" ~doc ~requires ~srcs

let tty_width_test =
  let down_tty_width_srcs =
    [ `File ~/"src/down_tty_width.ml"; `File ~/"src/down_tty_width.mli" ]
  in
  let doc = "Test generated TTY width data" in
  let requires = [uucp] in
  let srcs = (`File (Fpath.v "tty_width/test.ml")) :: down_tty_width_srcs in
  B0_ocaml.exe "tty-width-test" ~doc ~requires ~srcs

(* Packs *)

let dev =
  B0_pack.make "tty-width" ~doc:"down TTY width support package" ~locked:false
    [tty_width_test; tty_width_gen]

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The down programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/down"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/down/doc/"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/down.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/down/issues"
    |> B0_meta.(add description_tags)
      ["dev"; "toplevel"; "repl"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build"
         "--dev-pkg" "%{dev}%"
         "--lib-dir" "%{lib}%"]]|}
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.1.0"|};
        "uucp", {|dev|}]
  in
  B0_pack.make "default" ~doc:"down package" ~meta ~locked:true @@
  [down_lib]

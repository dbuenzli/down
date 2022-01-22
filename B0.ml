open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml libraries *)

let down = B0_ocaml.libname "down"
let down_nattop = B0_ocaml.libname "down.nattop"
let uucp = B0_ocaml.libname "uucp"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

(* Down libraries *)

let down_tty_width_srcs =
  Fpath.[ `File (v "src/down_tty_width.ml");
          `File (v "src/down_tty_width.mli") ]

let base_srcs =
  Fpath.(`File (v "src/down.mli") :: `File (v "src/down.ml") ::
         `File (v "src/down_std.mli") :: `File (v "src/down_std.ml") ::
         `File (v "src/down_stubs.c") ::
         down_tty_width_srcs)

let down_lib =
  let doc = "An OCaml toplevel upgrade" in
  let requires = [compiler_libs_toplevel] in
  let srcs =
    Fpath.(`File (v "src/down_top.mli") :: `File (v "src/down_top.ml") ::
           base_srcs)
  in
  B0_ocaml.lib down ~doc ~requires ~srcs

let down_nattop_lib =
  let doc = "Down ocamlnat support" in
  let requires = [compiler_libs_toplevel] in
  let srcs =
    Fpath.(`File (v "src/down_nattop.mli") :: `File (v "src/down_nattop.mli") ::
           base_srcs)
  in
  B0_ocaml.lib down_nattop ~doc ~requires ~srcs

(* TTY width data generation *)

let tty_width_gen =
  let doc = "Generate OCaml source for TTY width data" in
  let requires = [uucp] in
  let srcs = [`File (Fpath.v "tty_width/gen.ml")] in
  B0_ocaml.exe "tty-width-gen" ~doc ~requires ~srcs

let tty_width_test =
  let doc = "Test generated TTY width data" in
  let requires = [uucp] in
  let srcs = (`File (Fpath.v "tty_width/test.ml")) :: down_tty_width_srcs in
  B0_ocaml.exe "tty-width-test" ~doc ~requires ~srcs

(* Packs *)

let dev =
  B0_pack.v "tty-width" ~doc:"down TTY width support package" ~locked:false @@
  [tty_width_test; tty_width_gen]

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The down programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/down"
    |> add online_doc "https://erratique.ch/software/down/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/down.git"
    |> add issues "https://github.com/dbuenzli/down/issues"
    |> add description_tags ["dev"; "toplevel"; "repl"; "org:erratique"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build"
         "--dev-pkg" "%{dev}%"
         "--lib-dir" "%{lib}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uucp", {|dev|}]
    |> add B0_opam.Meta.install {|
      # Following is only to deal with
      # https://caml.inria.fr/mantis/view.php?id=7808
      [["install" "-d" "%{lib}%/ocaml/"]
       ["install" "src/down.top" "src/down.nattop" "%{lib}%/ocaml/"]]|}
  in
  B0_pack.v "default" ~doc:"down package" ~meta ~locked:true @@
  [down_lib; down_nattop_lib]

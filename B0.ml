open B0_kit.V000
open B00_std
open Result.Syntax

(* Packs *)

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
      [ "ocaml", {|>= "4.04.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|}]
    |> add B0_opam.Meta.install {|
      # Following is only to deal with
      # https://caml.inria.fr/mantis/view.php?id=7808
      [["install" "-d" "%{lib}%/ocaml/"]
       ["install" "src/down.top" "src/down.nattop" "%{lib}%/ocaml/"]]|}
  in
  B0_pack.v "default" ~doc:"down package" ~meta ~locked:true @@
  B0_unit.list ()

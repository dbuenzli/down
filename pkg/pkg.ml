#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let lib_dir =
  let doc = "Use $(docv) as the lib directory" in
  let absent () =
    let opam = Conf.tool "opam" `Host_os in
    OS.Cmd.(run_out Cmd.(opam % "config" % "var" % "lib") |> to_string)
  in
  Conf.(discovered_key "lib-dir" fpath ~absent ~doc)

let top_config c = match Conf.build_context c with
| `Dev -> Ok ()
| `Pin | `Distrib ->
    let lib_dir = Conf.value c lib_dir in
    let subst_lib_dir file =
      OS.File.read file >>= fun contents ->
      OS.File.write_subst file ["LIBDIR", lib_dir] contents
    in
    subst_lib_dir "src/down.top" >>= fun () ->
    subst_lib_dir "src/down.nattop"

let pre c = top_config c
let build = Pkg.build ~pre ()

let has_ocamlnat =
  let absent () = OS.Cmd.exists (Cmd.v "ocamlnat") in
  let doc = "Compile with ocamlnat support" in
  Conf.discovered_key ~doc "with-ocamlnat" Conf.bool ~absent

let () =
  Pkg.describe ~build "down" @@ fun c ->
  let has_ocamlnat = Conf.value c has_ocamlnat in
  Ok [ Pkg.mllib ~api:["Down"] "src/down.mllib";
       Pkg.mllib ~cond:has_ocamlnat ~api:["Down"] "src/down_nattop.mllib";
       Pkg.clib "src/libdown_stubs.clib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
       Pkg.toplevel "src/down.top";
       Pkg.toplevel ~cond:has_ocamlnat "src/down.nattop";
       Pkg.test "test/test";
       Pkg.test "test/test_input"; ]

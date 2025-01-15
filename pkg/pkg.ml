#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let lib_dir =
  let doc = "Use $(docv) as the lib directory" in
  let absent () =
    let opam = Conf.tool "opam" `Host_os in
    OS.Cmd.(run_out Cmd.(opam % "var" % "lib") |> to_string)
  in
  Conf.(discovered_key "lib-dir" fpath ~absent ~doc)

let top_config c = match Conf.build_context c with
| `Dev -> Ok ()
| `Pin | `Distrib ->
    let lib_dir = String.escaped (Conf.value c lib_dir) in
    let subst_lib_dir file =
      OS.File.read file >>= fun contents ->
      OS.File.write_subst file ["LIBDIR", lib_dir] contents
    in
    subst_lib_dir "src/down.top"

let pre c = top_config c
let build = Pkg.build ~pre ()

let () =
  Pkg.describe ~build "down" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Down"] "src/down.mllib";
       Pkg.clib "src/libdown_stubs.clib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
       Pkg.toplevel "src/down.top"; ]

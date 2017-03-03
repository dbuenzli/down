open Ocamlbuild_plugin
open Command

let lib s = match !Ocamlbuild_plugin.Options.ext_lib with
 | "" -> s ^ ".a"
 | x -> s ^ "." ^ x

let () =
  dispatch begin function
  | After_rules ->

      dep ["record_down_stubs"] [lib "src/libdown_stubs"];

      flag_and_dep
        ["link"; "ocaml"; "link_down_stubs"]
        (P (lib "src/libdown_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_down_stubs"]
        (S ([A "-dllib"; A "-ldown_stubs"]));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_down_stubs"]
        (S ([A "-cclib"; A "-ldown_stubs"]));

      ocaml_lib ~tag_name:"use_down_stubs"
        ~dir:"src" "src/down";

      flag ["link"; "ocaml"; "use_down_stubs"]
        (S [A "-ccopt"; A "-Lsrc"]);
  | _ -> ()
  end

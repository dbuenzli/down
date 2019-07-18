(*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Down_std

let err str = prerr_endline str; exit 1
let () = match Tty.cap with
| `None -> err "No ANSI capability detected."
| `Ansi ->
    match Stdin.set_raw_mode true with
    | false -> err "Could not set stdin in raw mode"
    | true ->
        let w = Tty.width Stdin.readc in
        let welcome = Printf.sprintf "Welcome! Your width is %d. Ding!\r\n" w in
        Tty.output welcome;
        Tty.output Tty.ding;
        let rec loop () = match Tty.input Stdin.readc with
        | None -> print_endline "EOF Bye!\r"
        | Some i ->
            match i with
            | `Ctrl (`Key 0x63) (* c *) -> print_endline "Bye.\r"
            | `Ctrl (`Key 0x64) (* d *) -> print_endline "EOF Bye.\r"
            | _ ->
                print_endline (Format.asprintf "%a\r" Tty.pp_input i); loop ()
        in
        loop ()

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

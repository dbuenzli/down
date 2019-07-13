(*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Result values *)

module Result = struct
  let catch_sys_error fn = try fn () with Sys_error e -> Error e
  let map_error f = function Ok _ as v -> v | Error e -> Error (f e)
  let bind r f = match r with Ok v -> f v | Error _ as e -> e
end

(* Tries *)

module Trie = struct
  module type S = sig
    type elt
    type 'a t
    val is_empty : 'a t -> bool
    val empty : 'a t
    val value : 'a t -> 'a option
    val add : elt list -> 'a option -> 'a t -> 'a t
    val find : elt list -> 'a t -> 'a t
    val find_fork : elt list -> 'a t -> elt list * 'a t
  end
  module Make (T : Map.OrderedType) = struct
    type elt = T.t
    module Tmap = Map.Make (T)
    type 'a t = { v : 'a option; succs : 'a t Tmap.t }
    let empty = { v = None; succs = Tmap.empty }
    let is_empty t = t.v = None && Tmap.is_empty t.succs
    let value t = t.v
    let add es v t =
      let rec loop es v t = match es with
      | [] -> { t with v }
      | e :: es ->
          let t' = try Tmap.find e t.succs with Not_found -> empty in
          let succs = Tmap.add e (loop es v t') t.succs in
          { t with succs }
      in
      loop es v t

    let rec find es t = match es with
    | [] -> t
    | e :: es ->
        match Tmap.find e t.succs with
        | exception Not_found -> empty
        | t -> find es t

    let find_fork es t =
      let is_fork t = t.v <> None || Tmap.cardinal t.succs <> 1 in
      let rec find_es acc es t = match es with
      | [] -> acc, t
      | e :: es ->
          match Tmap.find e t.succs with
          | exception Not_found -> acc, empty
          | t -> find_es (e :: acc) es t
      in
      let rec kontinue acc t = match is_fork t with
      | true -> List.rev acc, t
      | false ->
          match Tmap.choose t.succs with
          | exception Not_found -> assert false
          | (e, t) -> kontinue (e :: acc) t
      in
      let acc, t = find_es [] es t in
      kontinue acc t
  end
end

(* Seemingly UTF-8 text. *)

module Txt = struct

  (* FIXME do a good review of this. Especially w.r.t. to the start vs
     before/after strategy and the boundary conditions (indices vs
     positions). *)

  (* Lines *)

  let is_eol = function '\n' | '\r' -> true | _ -> false

  let lines s = (* adapted from the stdlib's String.split_on_char *)
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = '\n' then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := if i <> 0 && String.unsafe_get s (i - 1) = '\r' then i - 1 else i
      end
    done;
    String.sub s 0 !j :: !r

  (* XXX something smart should likely be done for CRFL management here... *)

  let next_eol_pos s ~start =
    let rec loop s max i = match i > max || s.[i] = '\n' || s.[i] = '\r' with
    | true -> i
    | false  -> loop s max (i + 1)
    in
    loop s (String.length s - 1) start

  let prev_eol_pos s ~start =
    let rec loop s i =
      if i <= 0 then 0 else
      if s.[i] = '\n' || s.[i] = '\r' then i else loop s (i - 1)
    in
    loop s start

  let prev_sol_pos s ~before =
    let rec loop s i =
      if i < 0 then 0 else
      if s.[i] = '\n' || s.[i] = '\r' then i + 1 else loop s (i - 1)
    in
    loop s (before - 1)

  (* UTF-8 uchars *)

  let uchar_may_start c = (* note: non-exact on invalid UTF-8 *)
    Char.code c land 0xC0 <> 0x80

  let rec sync_uchar_forward s ~max ~start:i =
    if i > max then i else
    if uchar_may_start s.[i] then i else
    sync_uchar_forward s ~max ~start:(i + 1)

  let rec sync_uchar_backward s ~start:i =
    if i <= 0 then 0 else
    if uchar_may_start s.[i] then i else
    sync_uchar_backward s ~start:(i - 1)

  let utf_8_decode_len c = match Char.code c with
  | b when b <= 0x7F -> 1 | b when b <= 0xBF -> 1
  | b when b <= 0xDF -> 2 | b when b <= 0xEF -> 3
  | b when b <= 0xF7 -> 4 | _ -> 1

  let uchar_count ?(start = 0) s =
    let max = String.length s - 1 in
    if max < 0 then 0 else
    let start' = sync_uchar_forward s ~max ~start in
    let count = if start' = start then 0 else 1 in
    let rec loop s max count i =
      if i > max then count else
      let skip = utf_8_decode_len s.[i] in
      loop s max (count + 1) (i + skip)
    in
    loop s max count start'

  (* Words *)

  let is_ascii_white = function
  | ' ' | '\t' | '\n' | '\x0B' | '\x0C' | '\r' -> true | _ -> false

  let rec fwd_skip_white s max i =
    if i > max || not (is_ascii_white s.[i]) then i else
    fwd_skip_white s max (i + 1)

  let rec fwd_skip_non_white s max i =
    if i > max || is_ascii_white s.[i] then i else
    fwd_skip_non_white s max (i + 1)

  let rec back_skip_white s i =
    if i <= 0 then 0 else
    if not (is_ascii_white s.[i]) then i else
    back_skip_white s (i - 1)

  let rec back_skip_non_white s i =
    if i <= 0 then 0 else
    if is_ascii_white s.[i] then i else
    back_skip_non_white s (i - 1)

  let next_past_eow_pos s ~start =
    let max = String.length s - 1 in
    fwd_skip_non_white s max (fwd_skip_white s max start)

  let prev_sow_pos s ~start =
    let i = back_skip_white s (start - 1) in
    if i = 0 then 0 else
    let i = back_skip_non_white s i in
    if is_ascii_white s.[i] then i + 1 else i

  (* Grapheme clusters. TODO the following needs to be adjusted with wcwidth.
     As far as data goes, try with a byte trie on UTF-8 encoded uchar. *)

  let gc_count ?start s = uchar_count ?start s
  let gc_byte_len s ~start =
    let i = sync_uchar_backward s ~start in
    utf_8_decode_len s.[i]

  let gc_next_pos s ~after ~count =
    let max = String.length s - 1 in
    if after > max then max + 1 else
    if count <= 0 then after else
    let start = sync_uchar_forward s ~max ~start:after in
    let count = if start = after then count else count - 1 (* count next *) in
    let rec loop s max count i =
      if i > max || count <= 0 then i else
      let skip = utf_8_decode_len s.[i] in
      loop s max (count - 1) (i + skip)
    in
    loop s max count start

  let gc_prev_pos s ~before ~count =
    let max = String.length s - 1 in
    if before = 0 then 0 else
    if count <= 0 then before else
    let start = if before > max then max else before in
    let count = if start = before then count else count - 1 (* count last *) in
    let start = sync_uchar_backward s ~start in
    let rec loop s count i =
      if i <= 0 then 0 else
      if count <= 0 then i else
      let i = sync_uchar_backward s ~start:(i - 1) in
      loop s (count - 1) i
    in
    loop s count start

  let gc_to_prev_eol_pos s ~before =
    if before = 0 then 0, 0 else
    let rec loop s count i =
      if s.[i] = '\n' then i, count else
      if i = 0 then 0, count + 1 else
      let i = gc_prev_pos s ~before:i ~count:1 in
      loop s (count + 1) i
    in
    loop s 0 (before - 1)
end

(* OS interaction *)

let cmd_run ?stdout ?stderr cmd =
  let err exit cmd = Error (exit, strf "exited with %d: %s\n" exit cmd) in
  let line ?stdout ?stderr cmd =
    let cmd = List.map Filename.quote cmd in
    let cmd = String.concat " " cmd in
    let redirect fd f = strf " %d>%s" fd (Filename.quote f) in
    let stdout = match stdout with None -> "" | Some f -> redirect 1 f in
    let stderr = match stderr with None -> "" | Some f -> redirect 2 f in
    let win_quote = if Sys.win32 then "\"" else "" in
    strf "%s%s%s%s%s" win_quote cmd stdout stderr win_quote
  in
  let line = line ?stdout ?stderr cmd in
  let exit = Sys.command line in
  if exit = 0 then Ok () else err exit line

module Env = struct
  let get var = match Sys.getenv var with
  | "" -> None | exception Not_found -> None | value -> Some value
end

module Dir = struct
  let config () = match Env.get "XDG_CONFIG_HOME" with
  | Some h -> Ok h
  | None ->
      match if Sys.win32 then Env.get "%LOCALAPPDATA%" else None with
      | Some h -> Ok h
      | None ->
          match Env.get (if Sys.win32 then "%HomePath%" else "HOME") with
          | Some h -> Ok (Filename.concat h ".config")
          | None -> Error "Could not determine a user configuration directory"

  let mkdir_win32 dir = ["mkdir"; dir]
  let mkdir_posix dir = ["mkdir"; "-p"; dir]
  let mkdir = if Sys.win32 then mkdir_win32 else mkdir_posix
  let create dir = Result.map_error snd @@ cmd_run (mkdir dir)
end

module File = struct
  let null = match Sys.os_type with "Win32" -> "NUL" | _ -> "/dev/null"
  let rename src ~dst =
    Result.catch_sys_error @@ fun () -> Ok (Sys.rename src dst)

  let delete file =
    Result.catch_sys_error @@ fun () -> Ok (Sys.remove file)

  let exists file =
    Result.catch_sys_error @@ fun () -> Ok (Sys.file_exists file)

  let with_io_chan close file chan fn =
    try let r = fn chan in close chan; Ok r with
    | e ->
        (try ignore (close chan) with Sys_error _ -> ());
        match e with
        | Sys_error err -> Error (strf "%s: %s" file err)
        | End_of_file -> Error (strf "%s: unexpected end of file" file)
        | e -> raise e

  let with_open_in file fn =
    Result.catch_sys_error @@ fun () ->
    let ic = open_in_bin file in
    with_io_chan close_in file ic fn

  let with_open_out file fn =
    Result.catch_sys_error @@ fun () ->
    let oc = open_out_bin file in
    with_io_chan close_out file oc fn

  let read file =
    with_open_in file @@ fun ic ->
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    Bytes.unsafe_to_string buf

  let write ~file s =
    with_open_out file @@ fun oc ->
    output_string oc s

  let set_content ~file s = Result.bind (exists file) @@ function
  | true ->
      let old = file ^ ".tmp" in
      Result.bind (rename file ~dst:old) @@ fun () ->
      Result.bind (write ~file s) @@ fun () ->
      delete old
  | false ->
      Result.bind (Dir.create (Filename.dirname file)) @@ fun () ->
      write ~file s

  let tmp ?(suff = "") () =
    Result.catch_sys_error @@ fun () ->
    let tmp = Filename.temp_file "ocaml" suff in
    at_exit (fun () -> try Sys.remove tmp with Sys_error e -> ());
    Ok tmp
end

module Cmd = struct
  type tool = string
  type t = string list
  let of_string s =
    let rec cleanup acc = function
    | "" :: args -> cleanup acc args
    | a :: args -> cleanup (String.trim a :: acc) args
    | [] -> List.rev acc
    in
    cleanup [] (String.split_on_char ' ' s)

  let run = cmd_run
  let test_cmd = if Sys.win32 then ["where"] else ["command"; "-v"]

  let exists cmd = match cmd with
  | [] -> Ok false
  | tool :: _ ->
      match run ~stdout:File.null ~stderr:File.null (test_cmd @ [tool]) with
      | Ok () -> Ok true
      | _ -> Ok false

  let must_exist cmd =
    Result.bind (exists cmd) @@ function
    | true -> Ok cmd
    | false -> Error (strf "%s: no such command" (List.hd cmd))

  let read cmd =
    let exitify = Result.map_error (fun s -> (255, s)) in
    Result.bind (exitify @@ File.tmp ~suff:"stdout" ()) @@ fun stdout ->
    Result.bind (run ~stdout cmd) @@ fun () ->
    exitify (File.read stdout)
end

module Tty = struct

  (* Terminal capabilities *)

  type cap = [ `None | `Ansi ]
  let find_cap () = match Sys.getenv "TERM" with
  | exception Not_found -> `None | "dumb" | "" -> `None | _ -> `Ansi

  let cap = find_cap ()

  (* ANSI escapes and styling *)

  type color =
  [ `Default | `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan
  | `White ]

  let sgr_base_int_of_color = function
  | `Black -> 0 | `Red -> 1 | `Green -> 2 | `Yellow -> 3  | `Blue -> 4
  | `Magenta -> 5 | `Cyan -> 6 | `White -> 7 | `Default -> 9

  let sgr_of_fg_color c = strf "%d" (30 + sgr_base_int_of_color c)
  let sgr_of_bg_color c = strf "%d" (40 + sgr_base_int_of_color c)

  type style =
  [ `Bold | `Faint | `Italic | `Underline | `Reverse | `Fg of color
  | `Bg of color ]

  let sgr_of_style = function
  | `Bold -> "01" | `Faint -> "02" | `Italic -> "03" | `Underline -> "04"
  | `Reverse -> "07" | `Fg c -> sgr_of_fg_color c | `Bg c -> sgr_of_bg_color c

  let sgrs_of_styles styles = String.concat ";" (List.map sgr_of_style styles)
  let styled_str cap styles s = match cap with
  | `None -> s | `Ansi -> strf "\027[%sm%s\027[m" (sgrs_of_styles styles) s

  (* Terminal output *)

  let output s = print_string s; flush stdout
  let ding = "\x07"
  let newline = "\r\n"
  let clear_row = "\x1B[2K"
  let cursor_up n =
    if n = 0 then "" else String.concat "" ["\x1B["; string_of_int n; "A"]

  let cursor_down n =
    if n = 0 then "" else String.concat "" ["\x1B["; string_of_int n; "B"]

  let cursor_forward n =
    if n = 0 then "" else String.concat "" ["\x1B["; string_of_int n; "C"]

  let cursor_origin = "\x1B[H"
  let clear_screen = "\x1B[2J"

  (* Terminal input *)

  type input =
  [ `Arrow of [ `Up | `Down | `Left | `Right ] | `Backspace | `Bytes of string
  | `Ctrl of int | `Delete | `End | `Enter | `Escape | `Function of int | `Home
  | `Meta of int | `Page of [ `Up | `Down ] | `Tab | `Unknown of string ]

  let pp_input ppf i =
    let pp = Format.fprintf in
    let dir_to_string = function
    | `Left -> "left " | `Right -> "right" | `Up -> "up   " | `Down -> "down "
    in
    let pp_char ppf c = match c with
    | c when c <= 0x20 || c >= 0x80 -> pp ppf "\\x%02X" c
    | 0x7F -> pp ppf "DEL"
    | c -> pp ppf "%c" (Char.chr c)
    in
    match i with
    | `Arrow dir -> pp ppf "%s" (dir_to_string dir)
    | `Backspace -> pp ppf "DEL"
    | `Bytes b ->
        if String.length b = 1 && Char.code b.[0] < 0x20
        then pp ppf "\"\\x%02X\"" (Char.code b.[0])
        else pp ppf "\"%s\"" b
    | `Ctrl c -> pp ppf "C-%a" pp_char c
    | `Delete -> pp ppf "delete"
    | `End -> pp ppf "end"
    | `Enter -> pp ppf "enter"
    | `Escape -> pp ppf "escape"
    | `Function n -> pp ppf "f%d" n
    | `Home -> pp ppf "home"
    | `Meta c -> pp ppf "M-%a" pp_char c
    | `Page dir -> pp ppf "page-%s" (dir_to_string dir)
    | `Tab -> pp ppf "tab"
    | `Unknown s -> pp ppf "unknown (%S)" s

  let read_esc readc = match readc () with
  | None -> `Escape
  | Some 0x5B (* '[' *) ->
      begin match readc () with
      | None -> `Unknown (strf "ESC[")
      | Some 0x41 -> `Arrow `Up
      | Some 0x42 -> `Arrow `Down
      | Some 0x43 -> `Arrow `Right
      | Some 0x44 -> `Arrow `Left
      | Some 0x46 -> `End
      | Some 0x48 -> `Home
      | Some b when 0x30 <= b && b <= 0x39 ->
          begin match readc () with
          | None -> `Unknown (strf "ESC[")
          | Some 0x7E ->
              begin match b with
              | 0x33 -> `Delete
              | c -> `Unknown (strf "ESC[%c~" (Char.chr c))
              end
          | Some b' -> `Unknown (strf "ESC[%c%c" (Char.chr b) (Char.chr b'))
          end
      | Some b -> `Unknown (strf "ESC[%c" (Char.chr b))
      end
  | Some 0x4F ->
      begin match readc () with
      | None -> `Unknown (strf "ESC G")
      | Some b when 0x50 <= b && b <= 0x53 -> `Function (b - 0x4F)
      | Some b -> `Unknown (strf "ESC G %c" (Char.chr b))
      end
  | Some b when 0x20 <= b || b < 0x7F -> `Meta b
  | Some b -> `Unknown (strf "ESC %02X" b)

  let read_bytes readc first =
    let rec loop buf i = match i < Bytes.length buf with
    | false -> Bytes.unsafe_to_string buf
    | true ->
        match readc () with
        | None -> Bytes.sub_string buf 0 i
        | Some b -> Bytes.set buf i (Char.chr b); loop buf (i + 1)
    in
    let first = Char.chr first in
    let buf = Bytes.create (Txt.utf_8_decode_len first) in
    Bytes.set buf 0 first; loop buf 1

  let input readc = match readc () with
  | None -> None
  | Some b ->
      let i = match b with
      | 0x09 -> `Tab
      | 0x0A -> `Bytes "\n"
      | 0x0D -> `Enter
      | 0x1B -> read_esc readc
      | b when b <= 0x1F -> `Ctrl (b + 0x60)
      | 0x7F -> `Backspace
      | b -> `Bytes (read_bytes readc b)
      in
      Some i

  (* Terminal width *)

  let width readc = (* defaults to 80 if something seems wrong *)
    let read_pos readc =
      let rec rreadc () = match readc () with None -> rreadc () | Some c -> c in
      let rec int ~stop acc = match rreadc () with
      | b when 0x30 <= b && b <= 0x39 -> int ~stop (10 * acc + (b - 0x30))
      | b when b = stop -> acc
      | _ -> 0
      in
      if rreadc () <> 0x1B then (0, 0) else
      if rreadc () <> 0x5B then (0, 0) else
      let row = int ~stop:0x3B 0 in
      let col = int ~stop:0x52 0 in
      col, row
    in
    let get_cursor_pos readc = output "\x1B[6n"; read_pos readc in
    try
      let x0, _ = get_cursor_pos readc in
      output "\x1B[100000C"; (* go far far right *)
      let x1, _ = get_cursor_pos readc in
      if x1 > x0 then output (strf "\x1B[%dD" (x1 - x0)); (* go back *)
      if x1 = 0 then raise Exit else x1
    with
    | Sys_error _ | Exit -> 80
end

module Stdin = struct
  external set_raw_mode : bool -> bool = "ocaml_down_stdin_set_raw_mode"
  external readc : unit -> int = "ocaml_down_stdin_readc"
  let readc () = match readc () with
  | -1 | -2 -> None | -3 -> raise (Sys_error "stdin read error") | n -> Some n

  let () =
    let disable_raw () = ignore (set_raw_mode false) in
    at_exit disable_raw
end

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let kpf = Format.kfprintf
  let str = Format.asprintf
  let string = Format.pp_print_string
  let sp ppf _ = Format.pp_print_space ppf ()
  let any fmt ppf _ = pf ppf fmt
  let list ?sep:pp_sep pp_v = Format.pp_print_list ?pp_sep pp_v
  let text = Format.pp_print_text
  let tty styles pp_v ppf v = match Tty.cap with
  | `None -> pp_v ppf v
  | `Ansi ->
      let reset ppf = pf ppf "@<0>%s" "\027[m" in
      let styles = Tty.sgrs_of_styles styles in
      kpf reset ppf "@<0>%s%a" (strf "\027[%sm" styles) pp_v v
end

module Editor = struct
  let find () = match Env.get "EDITOR" with
  | Some cmd -> Ok (Cmd.of_string cmd)
  | None ->
      match Env.get "VISUAL" with
      | Some cmd -> Ok (Cmd.of_string cmd)
      | None -> Error "No editor found in VISUAL or EDITOR env vars."

  let edit_string ~ext s =
    Result.bind (find ()) @@ fun editor ->
    Result.bind (File.tmp ~suff:ext ()) @@ fun tmp ->
    Result.bind (if s = "" then Ok () else File.write tmp s) @@ fun () ->
    Result.bind (Result.map_error snd @@ Cmd.run (editor @ [tmp])) @@ fun () ->
    Result.bind ( File.read tmp) @@ fun txt ->
    Ok (String.trim txt)

  let edit_file file =
    Result.bind (find ()) @@ fun editor ->
    Result.map_error snd @@ Cmd.run (editor @ [file])
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers

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

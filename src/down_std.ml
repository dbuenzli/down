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

  let find_next ~sat s ~start =
    let rec loop s max i =
      if i > max then String.length s else
      if sat s.[i] then i else loop s max (i + 1)
    in
    let max = String.length s - 1 and i = if start < 0 then 0 else start in
    loop s max i

  let find_prev ~sat s ~start =
    let rec loop s i =
      if i < 0 then 0 else
      if sat s.[i] then i else loop s (i - 1)
    in
    let max = String.length s - 1 in
    let i = if start > max then max else start in
    loop s i

  let keep_next_len ~sat s ~start =
    let rec loop s max i count =
      if i > max then count else
      if sat s.[i] then loop s max (i + 1) (count + 1) else count
    in
    let max = String.length s - 1 and i = if start < 0 then 0 else start in
    loop s max i 0

  let keep_prev_len ~sat s ~start =
    let rec loop s i count =
      if i < 0 then count else
      if sat s.[i] then loop s (i - 1) (count + 1) else count
    in
    let max = String.length s - 1 in
    let i = if start > max then max else start in
    loop s i 0

  (* Lines *)

  let lines s =
    (* adapted from the stdlib's String.split_on_char, handles CR, CRLF and
       LF line ends. *)
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = '\n' then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := if i <> 0 && String.unsafe_get s (i - 1) = '\r' then i - 1 else i
      end
    done;
    String.sub s 0 !j :: !r

  (* XXX something smarter should likely be done for CRFL management here...
     XXX Maybe look only for '\n' will do ? *)

  let is_eol = function '\n' | '\r' -> true | _ -> false
  let find_next_eol s ~start = find_next ~sat:is_eol s ~start
  let find_prev_eol s ~start = find_prev ~sat:is_eol s ~start
  let find_prev_sol s ~start =
    let i = find_prev_eol s ~start in
    if String.length s = 0 || (i = 0 && not (is_eol s.[i])) then 0 else i + 1

  (* UTF-8 uchars *)

  let utf_8_decode_len c = match Char.code c with
  | b when b <= 0x7F -> 1 | b when b <= 0xBF -> 1
  | b when b <= 0xDF -> 2 | b when b <= 0xEF -> 3
  | b when b <= 0xF7 -> 4 | _ -> 1

  let is_utf_8_decode c = Char.code c land 0xC0 <> 0x80
  let find_next_utf_8_decode s ~start = find_next ~sat:is_utf_8_decode s ~start
  let find_prev_utf_8_decode s ~start = find_prev ~sat:is_utf_8_decode s ~start

  (* White *)

  let is_white = function
  | ' ' | '\t' | '\n' | '\x0B' | '\x0C' | '\r' -> true | _ -> false

  let find_next_white s ~start = find_next ~sat:is_white s ~start
  let find_prev_white s ~start = find_prev ~sat:is_white s ~start

  (* In general it's unwise to use the following functions on UTF-8
     since they might end up on continuation bytes. But it's ok the
     way we use them for segmenting words below. *)

  let is_non_white c = not (is_white c)
  let find_next_non_white s ~start = find_next ~sat:is_non_white s ~start
  let find_prev_non_white s ~start = find_prev ~sat:is_non_white s ~start

  (* Words *)

  let find_next_after_eow s ~start =
    find_next_white s ~start:(find_next_non_white s ~start)

  let find_prev_sow s ~start =
    let i = find_prev_white s ~start:(find_prev_non_white s ~start) in
    if String.length s = 0 || (i = 0 && is_non_white s.[i]) then 0 else i + 1

  (* Grapheme clusters and TTY width *)

  let tty_width s ~start = match s.[start] = '\n' with
  | true -> 1
  | false -> Down_tty_width.of_utf_8 s ~start

  let find_next_gc_and_tty_width s ~after =
    let rec loop s max w i =
      if i > max then String.length s, w else
      let iw = tty_width s ~start:i in
      if w >= 1 && iw <> 0 then i, w else
      loop s max (w + iw) (i + utf_8_decode_len s.[i])
    in
    let max = String.length s - 1 and i = if after < 0 then 0 else after in
    loop s max 0 i

  let find_next_gc s ~after = fst (find_next_gc_and_tty_width s ~after)
  let find_prev_gc s ~before =
    let rec loop s w i =
      if i <= 0 then 0 else
      let i = find_prev_utf_8_decode s ~start:(i - 1) in
      let w = w + tty_width s ~start:i in
      if w >= 1 then i else loop s w i
    in
    let len = String.length s in
    let i = if before > len then len else before in
    loop s 0 i

  let find_prev_eol_and_tty_width s ~before =
    let rec loop s w i =
      if i <= 0 then 0, w else
      let i = find_prev_utf_8_decode s ~start:(i - 1) in
      if is_eol s.[i] then i, w else
      let w = w + tty_width s ~start:i in
      loop s w i
    in
    let len = String.length s in
    let i = if before > len then len else before in
    loop s 0 i

  let find_next_tty_width_or_eol s ~start ~w =
    let rec loop s max w i =
      if i > max then String.length s else
      if w <= 0 || is_eol s.[i] then i else
      let i, gc_w = find_next_gc_and_tty_width s ~after:i in
      loop s max (w - gc_w) i
    in
    let max = String.length s - 1 and i = if start < 0 then 0 else start in
    loop s max w i
end

(* Text made of entries separated by a special line. *)

module Txt_entries = struct
  let nl = if Sys.win32 then "\r\n" else "\n"
  let to_string ~sep es = String.concat (Printf.sprintf "%s%s%s" nl sep nl) es
  let of_string ~sep s =
    let add_entry acc lines =
      let e = String.concat nl (List.rev lines) in
      if e = "" then acc else e :: acc
    in
    let rec loop acc curr = function
    | [] -> List.rev (add_entry acc curr)
    | l :: ls ->
        if String.equal (String.trim l) sep
        then loop (add_entry acc curr) [] ls
        else loop acc (l :: curr) ls
    in
    loop [] [] (Txt.lines s)
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
      match if Sys.win32 then Env.get "%APPDATA%" else None with
      | Some h -> Ok h
      | None ->
          match Env.get "HOME" with
          | Some h -> Ok (Filename.concat h ".config")
          | None -> Error "Could not determine a user configuration directory"

  let mkdir_win32 dir = ["mkdir"; dir]
  let mkdir_posix dir = ["mkdir"; "-p"; dir]
  let mkdir = if Sys.win32 then mkdir_win32 else mkdir_posix
  let create dir = Result.map_error snd @@ cmd_run (mkdir dir)

  let exists dir =
    Result.catch_sys_error @@ fun () ->
    Ok Sys.(file_exists dir && is_directory dir)

  let contents dir =
    Result.catch_sys_error @@ fun () ->
    let contents = Array.to_list (Sys.readdir dir) in
    Ok (List.rev_map (Filename.concat dir) contents)
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

  let rec sgr_base_int_of_color = function
  | `Black -> 0 | `Red -> 1 | `Green -> 2 | `Yellow -> 3  | `Blue -> 4
  | `Magenta -> 5 | `Cyan -> 6 | `White -> 7 | `Default -> 9
  | `Hi (#color as c) -> 60 + sgr_base_int_of_color c

  let sgr_of_fg_color c = strf "%d" (30 + sgr_base_int_of_color c)
  let sgr_of_bg_color c = strf "%d" (40 + sgr_base_int_of_color c)

  type style =
  [ `Bold | `Faint | `Italic | `Underline | `Reverse
  | `Fg of [ color | `Hi of color ]
  | `Bg of [ color | `Hi of color ]]

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

  type arrow = [ `Up | `Down | `Left | `Right ]
  type input =
  [ `Arrow of [ `Up | `Down | `Left | `Right ] | `Backspace | `Bytes of string
  | `Ctrl of [ `Key of int | `Arrow of arrow] | `Delete | `End | `Enter
  | `Escape | `Function of int | `Home | `Meta of int
  | `Page of [ `Up | `Down ] | `Shift of [`Arrow of arrow ] | `Tab
  | `Unknown of string ]

  let pp_input ppf i =
    let pp = Format.fprintf in
    let dir_to_string = function
    | `Left -> "left " | `Right -> "right" | `Up -> "up   " | `Down -> "down "
    in
    let pp_char ppf c = match c with
    | c when c <= 0x20 || c >= 0x80 -> pp ppf "\\x%02X" c
    | 0x7F -> pp ppf "backspace"
    | c -> pp ppf "%c" (Char.chr c)
    in
    match i with
    | `Arrow dir -> pp ppf "%s" (dir_to_string dir)
    | `Backspace -> pp ppf "backspace"
    | `Bytes b ->
        if String.length b = 1 && Char.code b.[0] < 0x20
        then pp ppf "\"\\x%02X\"" (Char.code b.[0])
        else pp ppf "%S" b
    | `Ctrl (`Key c) -> pp ppf "C-%a" pp_char c
    | `Ctrl (`Arrow dir) -> pp ppf "C-%s" (dir_to_string dir)
    | `Delete -> pp ppf "delete"
    | `End -> pp ppf "end"
    | `Enter -> pp ppf "enter"
    | `Escape -> pp ppf "escape"
    | `Function n -> pp ppf "f%d" n
    | `Home -> pp ppf "home"
    | `Meta c -> pp ppf "M-%a" pp_char c
    | `Page dir -> pp ppf "page-%s" (dir_to_string dir)
    | `Tab -> pp ppf "tab"
    | `Shift (`Arrow dir) -> pp ppf "shift-%s" (dir_to_string dir)
    | `Unknown s -> pp ppf "unknown (%S)" s

  let read_bracketed_paste readc =
    let err = `Unknown "bracketed paste error" in
    let i = Buffer.create 1024 in
    let addc = Buffer.add_char and adds = Buffer.add_string in
    let rec retry readc i b =
      if Int.equal b 0x1b then try_stop readc i else
      (addc i (Char.chr b); loop readc i)
    and try_stop readc i = match readc () with
    | None -> err
    | Some 0x5B (* '[' *) ->
        let pre = "\x1B[" in
        begin match readc () with
        | None -> err
        | Some 0x32 (* '2' *) ->
            begin match readc () with
            | None -> err
            | Some 0x30 (* '0' *) ->
                begin match readc () with
                | None -> err
                | Some 0x31 (* '1' *) ->
                    begin match readc () with
                    | None -> err
                    | Some 0x7E (* ~ *) -> `Bytes (Buffer.contents i)
                    | Some b ->
                        adds i pre; addc i '2'; addc i '0'; addc i '1';
                        retry readc i b
                    end
                | Some b -> adds i pre; addc i '2'; addc i '0'; retry readc i b
                end
            | Some b -> adds i pre; addc i '2'; retry readc i b
            end
        | Some b -> adds i pre; retry readc i b
        end
    | Some b -> addc i '\x1B'; retry readc i b
    and loop readc i = match readc () with
    | None -> err
    | Some 0x1b -> try_stop readc i
    | Some 0x0d (* CR *) -> addc i '\n'; loop readc i
    | Some b -> addc i (Char.chr b); loop readc i
    in
    loop readc i

  let read_esc readc : input = match readc () with
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
      | Some 0x31 ->
          let pre = "ESC[1" in
          begin match readc () with
          | None -> `Unknown pre
          | Some 0x3B (* ; *) ->
              begin match readc () with
              | None -> `Unknown (strf "%s;" pre)
              | Some (0x35 | 0x32 as m) ->
                  let mk dir =
                    if m = 0x35 then `Ctrl (`Arrow dir) else `Shift (`Arrow dir)
                  in
                  begin match readc () with
                  | None -> `Unknown (strf "%s;%c" pre (Char.chr m))
                  | Some 0x41 -> mk `Up
                  | Some 0x42 -> mk `Down
                  | Some 0x43 -> mk `Right
                  | Some 0x44 -> mk `Left
                  | Some b ->
                      `Unknown (strf "%s;%c%c" pre (Char.chr m) (Char.chr b))
                  end
              | Some b -> `Unknown (strf "%s;%c" pre (Char.chr b))
              end
          | Some b -> `Unknown (strf "%s%c" pre (Char.chr b))
          end
      | Some 0x32 ->
          let pre = "ESC[2" in
          begin match readc () with
          | None -> `Unknown pre
          | Some 0x30 (* 0 *) ->
              begin match readc () with
              | None -> `Unknown (strf "%s0" pre)
              | Some 0x30 (* 0 *) ->
                  begin match readc () with
                  | None -> `Unknown (strf "%s00" pre)
                  | Some 0x7E (* ~ *) -> read_bracketed_paste readc
                  | Some b -> `Unknown (strf "%s00%c" pre (Char.chr b))
                  end
              | Some b -> `Unknown (strf "%s0%c" pre (Char.chr b))
              end
          | Some b -> `Unknown (strf "%s%c" pre (Char.chr b))
          end
      | Some 0x33 ->
          let pre = "ESC[3" in
          begin match readc () with
          | None -> `Unknown pre
          | Some 0x7E -> `Delete
          | Some b -> `Unknown (strf "%s%c" pre (Char.chr b))
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
      | 0x7F | 0x08 -> `Backspace
      | b when b <= 0x1F -> `Ctrl (`Key (b + 0x60))
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

  let enable_bracketed_paste oc =
    let output_string_noerr oc s () = try output_string oc s; flush oc with
    | Sys_error _ -> ()
    in
    output_string_noerr oc "\x1B[?2004h" ();
    at_exit (output_string_noerr oc "\x1B[?2004l")

  let () =
    let disable_raw () = ignore (set_raw_mode false) in
    at_exit disable_raw
end

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let pr = Format.printf
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
    Result.bind (if s = "" then Ok () else File.write ~file:tmp s) @@ fun () ->
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

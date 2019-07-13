(*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Down_std

(* Prompt history *)

module Phistory = struct
  type t = { prev : string list; current : string; next : string list; }
  let v prev = { prev; current = ""; next = [] }
  let empty = v []
  let push e es =
    if e = "" then es else match es with
    | e' :: _ when String.equal e e' -> es
    | es -> e :: es

  let entries h = List.rev_append (push h.current h.next) h.prev
  let add h entry = match String.trim entry with
  | "" -> h
  | entry ->
      match entries h with
      | [] -> v [entry]
      | e :: _ as entries when String.equal entry e -> v entries
      | es -> v (entry :: es)

  let prev h current = match h.prev with
  | [] -> h, current
  | p :: ps ->
      let next = push (String.trim current) (push h.current h.next) in
      let next = if next = [] then [""] (* bottom can be empty *) else next in
      { prev = ps; current = p; next; }, p

  let next h current = match h.next with
  | [] -> h, current
  | n :: ns ->
      let prev = push (String.trim current) (push h.current h.prev) in
      { prev; current = n; next = ns }, n

  (* Serializing *)

  let nl = if Sys.win32 then "\r\n" else "\n"
  let to_string ~sep h =
    String.concat (Printf.sprintf "%s%s%s" nl sep nl) (entries h)

  let of_string ~sep s =
    let add_entry acc lines =
      let e = String.concat nl (List.rev lines) in
      if e = "" then acc else e :: acc
    in
    let rec loop acc curr = function
    | [] -> v @@ List.rev (add_entry acc curr)
    | l :: ls ->
        if String.equal (String.trim l) sep
        then loop (add_entry acc curr) [] ls
        else loop acc (l :: curr) ls
    in
    loop [] [] (Txt.lines s)
end

(* String editor *)

module Pstring = struct
  type t = { s : string; cursor : int; mark : int option }
  let v ?(cursor = 0) ?mark s = { s; cursor; mark }
  let empty = v ""
  let is_empty p = String.equal p.s ""
  let txt p = p.s
  let txt_range ~first:r0 ~last:r1 p = String.sub p.s r0 (r1 - r0 + 1)
  let cursor p = p.cursor
  let set_cursor cursor p = if p.cursor = cursor then p else { p with cursor }
  let set_mark mark p = { p with mark }
  let swap_cursor_and_mark p = match p.mark with
  | None -> p | Some m when m > String.length p.s -> { p with mark = None }
  | Some m  -> { p with cursor = m; mark = Some p.cursor }

  let completion_start p =
    let rec loop s i = match i < 0 || Txt.is_white s.[i] with
    | true -> let ret = i + 1 in if ret = p.cursor then None else Some ret
    | false -> loop s (i - 1)
    in
    loop p.s (p.cursor - 1)

  let soi p = set_cursor 0 p
  let eoi p = set_cursor (String.length p.s) p
  let sol p = set_cursor (Txt.find_prev_sol p.s ~start:(p.cursor - 1)) p
  let eol p = set_cursor (Txt.find_next_eol p.s ~start:p.cursor) p
  let next_char p = set_cursor (Txt.find_next_gc p.s ~after:p.cursor) p
  let prev_char p = set_cursor (Txt.find_prev_gc p.s ~before:p.cursor) p
  let next_word p = set_cursor (Txt.find_next_after_eow p.s ~start:p.cursor) p
  let prev_word p = set_cursor (Txt.find_prev_sow p.s ~start:(p.cursor - 1)) p

  let prev_line p =
    let nl, w = Txt.find_prev_eol_and_tty_width p.s ~before:p.cursor in
    if String.length p.s = 0 || (nl = 0 && not (Txt.is_eol p.s.[0])) then p else
    let prev_nl = Txt.find_prev_eol p.s ~start:(nl - 1) in
    let start = prev_nl + if Txt.is_eol p.s.[prev_nl] then 1 else 0 in
    let cursor = Txt.find_next_tty_width_or_eol p.s ~start ~w in
    set_cursor cursor p

  let next_line p =
    let _, w = Txt.find_prev_eol_and_tty_width p.s ~before:p.cursor in
    let next_nl = Txt.find_next_eol p.s ~start:(p.cursor + 1) in
    if next_nl = String.length p.s then p else
    let cursor = Txt.find_next_tty_width_or_eol p.s ~start:(next_nl + 1) ~w in
    set_cursor cursor p

  let subst ~start:r0 ~stop:r1 bytes p =
    let slen = String.length p.s in
    let blen = String.length bytes in
    if r0 = slen then { p with s = p.s ^ bytes; cursor = slen + blen } else
    let b = String.sub p.s 0 r0 in
    let a = String.sub p.s r1 (slen - r1) in
    let s = String.concat "" [b; bytes; a] in
    let mark = match p.mark with
    | None -> None | Some m when m <= slen + blen -> p.mark | Some _ -> None
    in
    { s; cursor = r0 + blen; mark }

  let insert bytes p = subst p.cursor p.cursor bytes p
  let delete_next_char p =
    if p.cursor = String.length p.s then p else
    let stop = Txt.find_next_gc p.s ~after:p.cursor in
    subst ~start:p.cursor ~stop "" p

  let delete_prev_char p =
    if p.cursor = 0 then p else
    let prev = Txt.find_prev_gc p.s ~before:p.cursor in
    subst ~start:prev ~stop:p.cursor "" p

  let kill_next_word p =
    let stop = Txt.find_next_after_eow p.s ~start:p.cursor in
    if stop = p.cursor then p, None else
    let kill = txt_range ~first:p.cursor ~last:(stop - 1) p in
    subst ~start:p.cursor ~stop "" p, Some kill

  let kill_prev_word p =
    if p.cursor = 0 then p, None else
    let prev = Txt.find_prev_sow p.s ~start:(p.cursor - 1) in
    let kill = txt_range ~first:prev ~last:(p.cursor - 1) p in
    subst ~start:prev ~stop:p.cursor "" p, Some kill

  let kill_to_sol p =
    let start = Txt.find_prev_sol p.s ~start:(p.cursor - 1) in
    if start = p.cursor - 1 then p, None else
    let kill = txt_range ~first:start ~last:(p.cursor - 1) p in
    subst ~start ~stop:p.cursor "" p, Some kill

  let kill_to_eol p =
    let stop = Txt.find_next_eol p.s ~start:p.cursor in
    if stop = p.cursor then p, None else
    let kill = txt_range ~first:p.cursor ~last:(stop - 1) p in
    subst ~start:p.cursor ~stop "" p, Some kill

  let kill_region p = match p.mark with
  | None -> p, None
  | Some mark ->
      if mark > String.length p.s then { p with mark = None }, None else
      if mark = p.cursor then p, None else
      let min, max = match p.cursor < mark with
      | true -> p.cursor, mark
      | false -> mark, p.cursor
      in
      let max = if max = String.length p.s then max - 1 else max in
      let kill = txt_range ~first:min ~last:max p in
      let cursor = min and mark = Some min in
      { (subst ~start:min ~stop:(max + 1) "" p) with cursor; mark}, Some kill

  let geometry ~tty_w ~margin_w p =
    (* Returns [rmax], (cr, cc, c_nl). [rmax] and [cr] are zero-based
       row indexes relative to prompt row for: the maximal row and the
       cursor row. [cc] is the cursor column and [c_nl] indicates if a
       newline has to be added for a wrapping cursor. *)
    let rec loop s max cursor cr cc r c i = match i > max with
    | true ->
        if cursor <> String.length s then r, (cr, cc, false) else
        if c mod tty_w = 0 then r + 1, (r + 1, 0, true) else r, (r, c, false)
    | false ->
        let r, c = if c mod tty_w = 0 then (* wrap *) r + 1, 0 else r, c in
        let cr, cc = if i = cursor then (r, c) else (cr, cc) in
        let nl = s.[i] = '\n' in
        let i, gc_w = Txt.find_next_gc_and_tty_width s ~after:i in
        let r, c = if nl then r + 1, margin_w else r, c + gc_w in
        loop s max cursor cr cc r c i
    in
    loop p.s (String.length p.s - 1) p.cursor 0 margin_w 0 margin_w 0
end

(* Down logging *)

let log_down_error fmt =
  Format.kasprintf (fun s -> Tty.output s) ("down: " ^^ fmt ^^ "\r\n")

let log_on_error ~use:v = function
| Error e -> log_down_error "%s" e; v | Ok v -> v

(* History *)

module History = struct
  let file () =
    Result.bind (Dir.config ()) @@ fun dir ->
    Ok (Filename.concat dir "ocaml/history.ml")

  let sep = "(**)"
  let current = ref (Phistory.v [])

  let load () =
    log_on_error ~use:current @@
    Result.map_error (Fmt.str "history load failed: %s") @@
    Result.bind (file ()) @@ fun file ->
    Result.bind (File.exists file) @@ function
    | false -> Ok current
    | true ->
        Result.bind (File.read file) @@ fun data ->
        Ok (current := Phistory.of_string ~sep data; current)

  let save () =
    log_on_error ~use:() @@
    Result.map_error (Fmt.str "history save failed: %s") @@
    Result.bind (file ()) @@ fun file ->
    File.set_content ~file (Phistory.to_string ~sep (!current))

  let edit () =
    log_on_error ~use:() @@
    Result.map_error (Fmt.str "history edit failed: %s") @@
    Result.bind (file ()) @@ fun file ->
    let h = Phistory.to_string ~sep !current in
    Result.bind (File.set_content ~file h) @@ fun () ->
    Result.bind (Editor.edit_file file) @@ fun () ->
    Result.bind (File.read file) @@ fun h ->
    Ok (current := Phistory.of_string ~sep h)

  let clear () = current := Phistory.v []; save ()
end

(* Sessions *)

module Session = struct
  type t = string list
  let dir () =
    Result.bind (Dir.config ()) @@ fun dir ->
    Ok (Filename.concat dir "ocaml/session")

  let sep = "(**)"
  let current = ref []
  let step = ref 0
  let list () = failwith "TODO"
  let reset () = failwith "TODO"
  let step () = failwith "TODO"
  let skip () = failwith "TODO"
  let back () = failwith "TODO"
  let exec s = failwith "TODO"
  let edit s = failwith "TODO"
  let rename src ~dst = failwith "TODO"
  let copy src ~dst = failwith "TODO"
end

(* Prompting *)

module Prompt = struct
  module I = struct type t = Tty.input let compare = compare end
  module Itrie = Trie.Make (I)
  type cmd = t -> [`Eoi | `Kont | `Break]
  and t =
    { mutable tty_w : int;
      readc : unit -> int option;
      output : string -> unit;
      has_answer : Tty.input -> t -> string option;
      complete : string -> (string * string list, string) result;
      history : (* mutable *) Phistory.t ref;
      mutable clipboard : string;
      mutable txt : Pstring.t;
      (* These are zero-based rows relat. to the prompt line for clearing. *)
      mutable last_cr : int; (* last cursor render row *)
      mutable last_max_r : int; (* last max render row *) }

  let has_answer input p =
    (* FIXME ocaml and utop are a bit weird. Try to sort that out.
       Notably is there does seem to be any good reason not to input
       successive ;; separated phrases, ocaml does that on .ml files. *)
    let has_semisemi s =
      let rec loop s max i in_str lastc = match i > max with
      | true -> false
      | false when not in_str && lastc = ';' && s.[i] = ';' -> true
      | false ->
          let is_quote = s.[i] = '"' && not (lastc = '\\') in
          let in_str = if is_quote then not in_str else in_str in
          loop s max (i + 1) in_str s.[i]
      in
      loop s (String.length s - 1) 0 false '\x00'
    in
    let txt = Pstring.txt p.txt in
    match input with
    | `Enter when has_semisemi txt -> Some ((String.trim txt) ^ "\n")
    | _ -> None

  let create
      ?(complete = fun w -> Ok (w, [])) ?(history = ref (Phistory.v []))
      ?(output = Tty.output) ~tty_w ~readc ()
    =
    { tty_w; readc; output; has_answer; complete; history; clipboard = "";
      txt = Pstring.empty; last_cr = 0; last_max_r = 0; }

  (* Rendering *)

  let ding p = p.output Tty.ding
  let newline p = p.output Tty.newline
  let error p fmt =
    let error = Tty.styled_str Tty.cap [`Bold; `Fg `Red] "Error" in
    let k msg = p.output (Printf.sprintf "\r\n%s: %s\r\n" error msg) in
    Format.kasprintf k fmt

  let clear_screen p =
    p.output (String.concat "" Tty.[clear_screen; cursor_origin])

  let clear_ui p =
    (* Go from cursor to max row and back to prompt row clearing lines. *)
    let rec go acc = function
    | 0 -> acc | up -> go (Tty.clear_row :: Tty.cursor_up 1 :: acc) (up - 1)
    in
    let down = p.last_max_r - p.last_cr and up = p.last_max_r in
    let acc = go (Tty.clear_row :: Tty.cursor_down down :: []) up in
    p.output (String.concat "" (List.rev ("\r" :: acc)));
    p.last_cr <- 0; p.last_max_r <- 0

  let prompt = "# "
  let margin = "  "
  let nl_margin = "\r\n  "
  let render_prompt ~active = match active with
  | true -> Tty.styled_str Tty.cap [`Fg `Green] prompt
  | false -> Tty.styled_str Tty.cap [`Faint; `Fg `Green] prompt

  let render_ui ?(active = true) p =
    let tty_w = p.tty_w and margin_w = String.length margin in
    let max_r, (cr, cc, c_nl) = Pstring.geometry ~tty_w ~margin_w p.txt in
    let add_line acc l = nl_margin :: l :: acc in
    let acc = [render_prompt ~active] in
    let acc = List.fold_left add_line acc (Txt.lines (Pstring.txt p.txt)) in
    let acc = "\r" :: List.tl acc (* remove exceeding nl_margin *) in
    let acc = if c_nl (* cursor wrapped *) then "\n" :: acc else acc in
    let acc = Tty.cursor_forward cc :: Tty.cursor_up (max_r - cr) :: acc in
    let ui = String.concat "" (List.rev acc) in
    clear_ui p; p.output ui; p.last_cr <- cr; p.last_max_r <- max_r

  let render_incomplete p prefix candidates =
    let render_candidate prefix c =
      (* Hackish. E.g. we don't actually get candidates one per line in case
         of module types. *)
      let blen = String.length c in
      if blen = 0 then "" else
      let sty sty s = Tty.styled_str Tty.cap sty s in
      let suf_style = [`Fg `Magenta] in
      let rst_style = [`Italic; `Faint] in
      if c.[0] = ' ' then sty rst_style c else
      match String.index c '\t' with
      | exception Not_found -> c (* should not happen but be robust *)
      | tab ->
          let rst_start = tab + 1 in
          let suf_start = String.length prefix in
          let pre = String.sub c 0 suf_start in
          let suf = String.sub c suf_start (tab - suf_start) in
          let rst = ":" ^ String.sub c rst_start (blen - rst_start) in
          Printf.sprintf "  %s%s%s" pre (sty suf_style suf) (sty rst_style rst)
    in
    let candidates = List.map (render_candidate prefix) candidates in
    render_ui ~active:false p;
    newline p; p.output (String.concat Tty.newline candidates); newline p

  (* Commands *)

  let set_txt_value p txt = p.txt <- Pstring.eoi (Pstring.v txt)
  let update_history op p =
    let h, txt = op !(p.history) (Pstring.txt p.txt) in
    if h == !(p.history) then ding p else (set_txt_value p txt; p.history := h)

  let update op p =
    let txt = op p.txt in
    if txt == p.txt (* bof *) then ding p else p.txt <- txt

  let update_with_kill op p =
    let txt, kill = op p.txt in
    let clipboard = match kill with None -> ding p; p.clipboard | Some s -> s in
    p.txt <- txt; p.clipboard <- clipboard

  let soi = update Pstring.soi
  let eoi = update Pstring.eoi
  let sol = update Pstring.sol
  let eol = update Pstring.eol
  let prev_char = update Pstring.prev_char
  let next_char = update Pstring.next_char
  let prev_word = update Pstring.prev_word
  let next_word = update Pstring.next_word
  let prev_line = update Pstring.prev_line
  let next_line = update Pstring.next_line
  let insert bytes = update (Pstring.insert bytes)
  let delete_next_char = update Pstring.delete_next_char
  let delete_prev_char = update Pstring.delete_prev_char
  let set_mark = update (fun p -> Pstring.set_mark (Some (Pstring.cursor p)) p)
  let swap_cursor_and_mark = update Pstring.swap_cursor_and_mark
  let kill_prev_word = update_with_kill Pstring.kill_prev_word
  let kill_next_word = update_with_kill Pstring.kill_next_word
  let kill_to_sol = update_with_kill Pstring.kill_to_sol
  let kill_to_eol = update_with_kill Pstring.kill_to_eol
  let kill_region = update_with_kill Pstring.kill_region
  let yank p =
    if p.clipboard = "" then ding p else
    let mark = Some (Pstring.cursor p.txt) and clip = p.clipboard in
    update (fun p -> Pstring.set_mark mark (Pstring.insert clip p)) p

  let edit p = match Editor.edit_string ~ext:".ml" (Pstring.txt p.txt) with
  | Error e -> error p "%s" e
  | Ok txt -> set_txt_value p txt

  let complete p = match Pstring.completion_start p.txt with
  | None -> ding p
  | Some start ->
      let set_subst p start old w =
        p.txt <- Pstring.subst start (start + String.length old) w p.txt
      in
      let word = Pstring.txt_range start (Pstring.cursor p.txt - 1) p.txt in
      match p.complete word with
      | Error e -> error p "%s" e
      | Ok (_, []) -> ding p
      | Ok (w, [_]) -> set_subst p start word w
      | Ok (w, cs) -> render_incomplete p w cs; set_subst p start word w

  let ctrl_d p =
    if Pstring.txt p.txt = "" then `Eoi else (delete_next_char p; `Kont)

  let add_history p s = p.history := Phistory.add !(p.history) s
  let prev_history = update_history Phistory.prev
  let next_history = update_history Phistory.next
  let break p = `Break
  let kont f p = f p; `Kont
  let cmds : (Tty.input list * cmd * string) list = [
    [`Home], kont soi, "move to start of input";
    [`End], kont eoi, "move to end of input";
    [`Ctrl 0x61 (* a *)], kont sol, "move to start of line";
    [`Ctrl 0x65 (* e *)], kont eol, "move to end of line";
    [`Ctrl 0x62 (* b *)], kont prev_char, "move to previous character";
    [`Ctrl 0x66 (* f *)], kont next_char, "move to next character";
    [`Arrow `Left], kont prev_char, "move to previous character";
    [`Arrow `Right], kont next_char, "move to next character";
    [`Meta 0x62 (* b *)], kont prev_word, "move to start of previous word";
    [`Meta 0x66 (* f *)], kont next_word, "move after the end of next word";
    (* FIXME should this also move in hist ? *)
    [`Ctrl 0x70 (* p *)], kont prev_line, "move to previous line";
    [`Ctrl 0x6E (* n *)], kont next_line, "move to next line";
    [`Arrow `Up], kont prev_history, "previous history entry";
    [`Arrow `Down], kont next_history, "next history entry";
    [`Backspace;], kont delete_prev_char, "delete previous character";
    [`Ctrl 0x64 (* d *)], ctrl_d,
    "delete next character or exit if input is empty";
    [`Ctrl 0x63 (* c *)], break, "abandon input";
    [`Ctrl 0x60 (* space ? *)], kont set_mark, "set the mark";
    [`Ctrl 0x78 (* x *);`Ctrl 0x78 (* x *)], kont swap_cursor_and_mark,
    "swap cursor and mark";
    [`Ctrl 0x79 (* y *)], kont yank, "yank";
    [`Ctrl 0x6B (* k *)], kont kill_to_eol, "kill to end of line";
    [`Ctrl 0x75 (* k *)], kont kill_to_sol, "kill to start of line";
    [`Meta 0x7F ], kont kill_prev_word, "kill to start of previous word";
    [`Meta 0x64 (* d *)], kont kill_next_word, "kill to end of next word";
    [`Ctrl 0x77 (* w *)], kont kill_region, "kill region";
    [`Ctrl 0x6C (* l *)], kont clear_screen, "clear screen";
    [`Tab], kont complete, "complete identifier";
    [`Ctrl 0x78 (* x *);`Ctrl 0x65 (* e *)], kont edit,
    "edit input with external program (VISUAL or EDITOR env var)" ]

  let pp_cmd ppf (is, _, doc) =
    let pp_is = Fmt.tty [`Bold] (Fmt.(list ~sep:sp Tty.pp_input)) in
    Fmt.pf ppf "@[%a  @[%a@]@]" pp_is is Fmt.text doc

  let cmd_trie =
    let add t (is, cmd, _doc) = Itrie.add is (Some cmd) t in
    List.fold_left add Itrie.empty cmds

  (* Event loop *)

  let ask p =
    let reset p = p.last_cr <- 0; p.last_max_r <- 0; p.txt <- Pstring.empty in
    let return p = render_ui ~active:false p; p.output Tty.newline in
    let rec loop p input_state =
      render_ui p;
      match Tty.input p.readc with
      | None ->
          (* Will mostly happen on EINTR and thus SIGWINCH *)
          p.tty_w <- Tty.width p.readc; loop p input_state
      | Some i ->
          match p.has_answer i p with
          | Some a -> (add_history p a; return p; `Answer a)
          | None ->
              let input_state = Itrie.find [i] input_state in
              match Itrie.value input_state with
              | None when Itrie.is_empty input_state ->
                  begin match i with
                  | `Bytes bytes -> insert bytes p; loop p cmd_trie
                  | `Enter -> insert "\n" p; loop p cmd_trie
                  | _ -> ding p; loop p cmd_trie
                  end
              | None -> loop p input_state
              | Some cmd ->
                  match cmd p with
                  | `Kont -> loop p cmd_trie
                  | `Break -> return p; `Break
                  | `Eoi -> `Eoi
    in
    (reset p; loop p cmd_trie)
end

(* Toploop interaction *)

module Complete = struct
  (* FIXME. POC hack via ocp-index, we likely want to that ourselves since we
     also need to peek in the OCaml toplevel symtable to be able to
     complete what the user defined and keep track of [open]s. *)

  module Ctrie = Trie.Make (Char)

  let string_to_list s =
    let rec go s i acc = if i < 0 then acc else go s (i - 1) (s.[i] :: acc) in
    go s (String.length s - 1) []

  let string_of_list l =
    let b = Buffer.create 255 in
    List.iter (Buffer.add_char b) l; (Buffer.contents b)

  let has_ocp_index =
    lazy begin match Cmd.exists ["ocp-index"] with
    | Error _ as e -> e
    | Ok true -> Ok ()
    | Ok false ->
        Error "Completion needs ocp-index. Try 'opam install ocp-index'."
    end

  let complete_cmd token = ["ocp-index"; "complete"; "-f"; "%q \t %t"; token ]
  let complete_word word results =
    let add_id acc r =
      let id = String.trim (List.hd (String.split_on_char '\t' r)) in
      if id = "" then acc else Ctrie.add (string_to_list id) (Some ()) acc
    in
    let ids = List.fold_left add_id Ctrie.empty results in
    let word, _ = Ctrie.find_fork (string_to_list word) ids in
    (string_of_list word, results)

  let finish_single_complete = function
  | "" -> ""
  | w when w.[String.length w - 1] = '.' -> w
  | w ->
      let path_start = match String.rindex w '.' with
      | exception Not_found -> 0 | i -> i + 1
      in
      if Char.uppercase_ascii w.[path_start] = w.[path_start]
      then w ^ "." (* Likely Module name path segment. *)
      else w ^ " " (* Likely Module structure item segment. *)

  let with_ocp_index = function
  | "" -> Ok ("", [])
  | w ->
      Result.bind (Lazy.force has_ocp_index) @@ fun () ->
      Result.bind (Result.map_error snd @@ Cmd.read (complete_cmd w)) @@
      fun cs -> match String.trim cs with
      | "" -> Ok (w, [])
      | s ->
          match complete_word w (Txt.lines s) with
          | w, ([_] as cs) -> Ok (finish_single_complete w, cs)
          | _ as ret -> Ok ret
end

let blit_toploop_buf s i b blen =
  let slen = String.length s in
  let slen_to_write = slen - i in
  let len = min blen slen_to_write in
  let snext = i + len in
  Bytes.blit_string s i b 0 len;
  len, (if snext < slen then Some (s, snext) else None)

let ocaml_readline = ref (fun _ _ _ -> assert false)
let down_readline p =
  let rem = ref None in
  fun prompt b len -> match !rem with
  | Some (s, i) ->
      let len, rem' = blit_toploop_buf s i b len in
      rem := rem'; (len, false)
  | None ->
      let rec loop p = match Prompt.ask p with
      | `Eoi -> History.save (); (0, true)
      | `Break -> Tty.output "Interrupted.\r\n"; loop p
      | `Answer ans ->
          let len, rem' = blit_toploop_buf ans 0 b len in
          rem := rem'; (len, false)
      in
      match Stdin.set_raw_mode true with
      | false -> !ocaml_readline prompt b len
      | true -> let r = loop p in ignore (Stdin.set_raw_mode false); r

let help () =
  let pp_help ppf () =
    Fmt.pf ppf "@[<v>%a:@, @[<v>%a@]@]@."
      (Fmt.tty [`Fg `Yellow] Fmt.string) "Available key bindings"
      (Fmt.list Prompt.pp_cmd) Prompt.cmds
  in
  pp_help Format.std_formatter ()

module Private = struct
  let pp_announce ppf () =
    Format.fprintf ppf
      "%a %%VERSION%% loaded. Tab complete %a for more info.@."
      Fmt.(tty [`Fg `Yellow] string) "Down"
      Fmt.(tty [`Bold] string) "Down.help ()"

  external sigwinch : unit -> int = "ocaml_down_sigwinch"

  let install_readline readline = match Tty.cap with
  | `None -> log_down_error "Disabled. No ANSI terminal capability detected."
  | `Ansi ->
      match Stdin.set_raw_mode true with
      | false -> log_down_error "Disabled. Raw mode setup for stdin failed."
      | true ->
          let tty_w = Tty.width Stdin.readc in
          ignore (Stdin.set_raw_mode false);
          let () =
            (* This is sufficient to interrupt the event loop on window size
               changes. *)
            let nop _ = () in
            Sys.set_signal (sigwinch ()) (Sys.Signal_handle nop)
          in
          let history = History.load () in
          let complete = Complete.with_ocp_index in
          let readc = Stdin.readc in
          let p = Prompt.create ~complete ~history ~tty_w ~readc () in
          ocaml_readline := !readline;
          readline := down_readline p;
          pp_announce Format.std_formatter ()
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
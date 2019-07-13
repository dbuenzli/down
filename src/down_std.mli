(*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Down standard needs. *)

(** Result values. *)
module Result : sig
  val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
  val bind : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
end

(** Simple tries with incremental lookup. *)
module Trie : sig
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
  module Make (T : Map.OrderedType) : S with type elt = T.t
end

(** UTF-8 text handling, possibly malformed. *)
module Txt : sig

  (** {1:lines Lines} *)

  val is_eol : char -> bool
  (** [is_eol] is [true] iff [c] is ['\r'] or ['\n']. *)

  val lines : string -> string list
  (** [lines s] splits [s] into CR, CRLF, LF lines separated lines. This is
      [[""]] on the empty string. *)

  val next_eol_pos : string -> start:int -> int
  (** [next_eol_pos s ~start] is either [Sys.max_string s] or the index
      of the byte at or after [start] that satisfies {!is_eol}. *)

  val prev_sol_pos : string -> before:int -> int
  (** [prev_sol_pos s ~start] is either [0] or the index of the byte
      after the byte before [before] that satisfies {!is_eol} *)

  val prev_eol_pos : string -> start:int -> int
  (** [next_eol s ~start] is either [0] or the index of the byte
      at or before [start] that satisfies {!is_eol}. *)

  (** {1:uchar UTF-8 encoded Unicode characters} *)

  val uchar_may_start : char -> bool
  (** [uchar_may_start c] is [true] iff [c] is not an UTF-8
      continuation byte. *)

  val utf_8_decode_len : char -> int
  (** [utf_8_decode_len b] is the length of an UTF-8 encoded Unicode
      character starting with byte [b]. The function returns is [1] for
      malformed or continuation bytes. *)

  val uchar_count : ?start:int -> string -> int
  (** [uchar_count ~start s] is the number of Unicode characters in
      [s] starting at [start] (defaults to [0]). *)

  (** {1:words Words} *)

  val is_ascii_white : char -> bool
  (** [is_ascii_white c] is [true] iff [c] is US-ASCII whitespace. *)

  val next_past_eow_pos : string -> start:int -> int
  (** [next_past_eow_pos] is the byte position after skipping forward
      first white and then non-white. The result may be [String.length s]. *)

  val prev_sow_pos : string -> start:int -> int
  (** [prev_sow_pos] is the byte position obtained by skipping backward
      first white and then non-white from [start - 1]. *)

  (** {1:gc Grapheme clusters}

      {b Note.} This is a simple notion of grapheme cluster based
      on {!Uucp.Break.tty_width_hint}. {b Note on Note.} That's not
      even true yet. *)

  val gc_count : ?start:int -> string -> int
  (** [gc_count ~start s] are the number of grapheme clusters
      in [s] starting at [s] (defaults to [0]). *)

  val gc_byte_len : string -> start:int -> int
  (** [gc_byte_len ~start s] is the length in bytes of the grapheme cluster
      starting at byte [start] in [s]. *)

  val gc_next_pos : string -> after:int -> count:int -> int
  (** [gc_next_pos s ~count ~after] is the byte position of the [count]h
      grapheme cluster after byte index [after] or [String.lenght s]
      if there were less than that. If [count] is [0] this is [after]. *)

  val gc_prev_pos : string -> before:int -> count:int -> int
  (** [gc_prev_idx s ~count ~before] is the byte position of [count]h
      grapheme cluster before byte index [before] or [0] if there
      were less than that. [before] can be [String.length s]. If [count]
      is [0] this is [before]. *)

  val gc_to_prev_eol_pos : string -> before:int -> int * int
  (** [gc_to_prev_eol_pos s ~start] is (i, gc_count) with [i] either
      0 or the byte index before [before] that satisfies {!is_eol} and
      [gc_count] the number of grapheme clusters skipped to get there.  *)
end

(** Environment variables. *)
module Env : sig
  val get : string -> string option
  (** [get var] is environment variable [var]. If the variable
      is empty [None] is returned. *)
end

(** Directories *)
module Dir : sig
  val config : unit -> (string, string) result
  (** [config ()] is the directory used to store user-specific program
      configuration. *)

  val create : string -> (unit, string) result
  (** [create dir] creates directory [dir]. *)
end

(** Files. *)
module File : sig

  val null : string
  (** [null] is an empty file that discards all writes. *)

  val exists : string -> (bool, string) result
  (** [exists file] checks for file (or directory) existence. *)

  val delete : string -> (unit, string) result
  (** [delete file] deletes file [file]. *)

  val rename : string -> dst:string -> (unit, string) result
  (** [rename src ~dst] renames [src] into [dst]. *)

  val read : string -> (string, string) result
  (** [read file] is the contents of file [file]. *)

  val write : file:string -> string -> (unit, string) result
  (** [write file d] writes [d] to [file]. *)

  val set_content : file:string -> string -> (unit, string) result
  (** [set_content ~file s] sets the contents of file [file] to [s].
      The path to the file is created if it doesn't exist. *)

  val tmp : ?suff:string -> unit -> (string, string) result
  (** [tmp ()] is a temporary file whose name ends with [suff]. *)
end

(** Executing commands *)
module Cmd : sig

  type t = string list
  (** The type for commands. *)

  val of_string : string -> t
  (** [of_string s] is a command from string [s]. *)

  val exists : t -> (bool, string) result
  (** [exists cmd] checks if the tool in [cmd] exists. *)

  val must_exist : t -> (t, string) result
  (** [must_exist cmd] checks the tool in [cmd] exists and fails
      with an error message otherwise. *)

  val run : ?stdout:string -> ?stderr:string -> t -> (unit, int * string) result
  (** [run ~stdout ~stderr c] run commands [c] and returns an error on non zero
      exit. [stdout] and [stderr] can be used to redirect the corresponding
      outputs to files. *)

  val read : t -> (string, int * string) result
  (** [read c] runs commands [c] and returns its standard input
      or non zero on exit. *)
end

(** Terminal interaction. *)
module Tty : sig

  (** {1:cap Capabilities} *)

  type cap = [ `None | `Ansi ]
  (** The type for capabilities. *)

  val cap : cap
  (** [cap] is the current terminal capability. This
        only uses environment variables to detect it. *)

  (** {1:ansi ANSI Styling} *)

  type color =
  [ `Default | `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan
  | `White ]
  (** The type for ANSI colors. *)

  type style =
  [ `Bold | `Faint | `Italic | `Underline | `Reverse | `Fg of color
  | `Bg of color ]
  (** The type for ANSI styles. *)

  val styled_str : cap -> style list -> string -> string
  (** [styled_str cap styles s] is [s] styled according to [cap] and
      [styles]. *)

  (** {1:out Output} *)

  val output : string -> unit
  (** [output s] outputs [s] on [stdout] and flushes it. *)

  val ding : string
  (** [ding] rings the bell. *)

  val newline : string
  (** [newline] is CRLF. *)

  val clear_row : string
  (** [clear_row] erases the row. *)

  val cursor_up : int -> string
  (** [cursor_up n] moves up [n] rows. *)

  val cursor_down : int -> string
  (** [cursor_down n] moves down [n] rowns. *)

  val cursor_forward : int -> string
  (** [cursor_forward n] moves cursor by [n] columns. *)

  val cursor_origin : string
  (** [cursor_origin] moves cursor to the top-left origin. *)

  val clear_screen : string
  (** [clear_screen] clears the screen. *)

  (** {1:input Input} *)

  type input =
  [ `Arrow of [ `Up | `Down | `Left | `Right ] | `Backspace | `Bytes of string
  | `Ctrl of int | `Delete | `End | `Enter | `Escape | `Function of int
  | `Home | `Meta of int | `Page of [ `Up | `Down ] | `Tab
  | `Unknown of string ]
  (** The type for user input. *)

  val input : (unit -> int option) -> input option
  (** [input readc] is user input read byte-by-byte using [readc]. *)

  val pp_input : Format.formatter -> input -> unit
  (** [pp_input] formats inputs. *)

  (** {1:witdh Width} *)

  val width : (unit -> int option) -> int
  (** [width readc] tries to termine the tty width using {!output} and
      [readc] to read the result. *)
end

(** Setup standard input for tty interaction.

    {b Note.} When the module is loaded an {!at_exit} handler
    is installed to make sure [set_raw_mode false] is called
    at the end of the program. *)
module Stdin : sig

  (** {1:raw Raw mode and character input} *)

  val set_raw_mode : bool -> bool
  (** [set_raw_mode raw] sets stdin raw mode according to [raw]
        returns [false] if the operation fails. *)

  val readc : unit -> int option
  (** [readc ()] reads a single byte from [stdin]. This is a
        blocking [read]. [None] is returned in case the operation
        failed for some reason. *)
end

(** Format module helpers. *)
module Fmt : sig

  type 'a t = Format.formatter -> 'a -> unit
  (** The type for value formatters. *)

  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  (** [pf] is {!Format.fprintf} *)

  val kpf :
    (Format.formatter -> 'a) ->
    Format.formatter -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kpf] is {!Format.kfprintf}. *)

  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  (** [str] is {!Fromat.asprintf}. *)

  val any : (unit, Format.formatter, unit) format -> 'a t
  (** [amy fmt] formats any value with [fmt]. *)

  val sp : 'a t
  (** [sp] is {!Format.pp_print_space}. *)

  val string : string t
  (** [string] is {!Format.pp_print_string}. *)

  val list : ?sep:unit t -> 'a t -> 'a list t
  (** [list] is {!Format.pp_print_list}. *)

  val text : string t
  (** [text] is {!Format.pp_print_text}. *)

  val tty : Tty.style list -> 'a t -> 'a t
  (** [tty style pp_v] formats [pp_v] with style [style]. *)
end

(** Editor interaction. *)
module Editor : sig

  val find : unit -> (Cmd.t, string) result
  (** [find ()] finds an editor command in EDITOR or VISUAL (in
      that order). Does not check it's runnable. *)

  val edit_string : ext:string -> string -> (string, string) result
  (** [edit_string ~ext s] edits strings [s] in a temporary file with
      extension [ext] with an editor found using [find ()]. The resulting
      string is {!String.trim}ed. *)

  val edit_file : string -> (unit, string) result
  (** [edit_file s] edits file [s] with an editor found using {!find}. *)
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

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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

(** UTF-8 text handling, possibly malformed.

    {b Note.} [start], [after] and [before] arguments can be out of
    bounds and in particular equal to the string length.  Finding
    forwards returns the string length if it cannot be found, finding
    backwards returns 0 if it cannot be found. *)
module Txt : sig

  val find_next : sat:(char -> bool) -> string -> start:int -> int
  (** [find_next ~sat s ~start] is either the [Sys.max_string s] or
      the index of the byte at or after [start] that satisfies [sat]. *)

  val find_prev : sat:(char -> bool) -> string -> start:int -> int
  (** [find_prev ~sat s ~start] is either the [0] or
      the index of the byte at or before [start] that satisfies [sat]. *)

  val keep_next_len : sat:(char -> bool) -> string -> start:int -> int
  (** [keep_next_len ~sat s ~start] is the number of consecutive
      next [sat] satisfying bytes starting at [start], included. *)

  val keep_prev_len : sat:(char -> bool) -> string -> start:int -> int
  (** [keep_prev_len ~sat s ~start] is the number of consecutive
      previous [sat] satisfying bytes starting at [start], included. *)

  (** {1:lines Lines} *)

  val lines : string -> string list
  (** [lines s] splits [s] into CR, CRLF, LF lines separated lines. This is
      [[""]] on the empty string. *)

  val is_eol : char -> bool
  (** [is_eol] is [true] iff [c] is ['\r'] or ['\n']. *)

  val find_next_eol : string -> start:int -> int
  (** [find_next_eol s ~start] is either [Sys.max_string s] or the index of
      the byte at or after [start] that satisfies {!is_eol}. *)

  val find_prev_eol : string -> start:int -> int
  (** [find_prev_eol s ~start] is either [0] or the index of the byte
      at or before [start] that satisfies {!is_eol}. *)

  val find_prev_sol : string -> start:int -> int
  (** [find_prev_sol s ~start] is either [0] or the position {e after} the
      byte at or before [start] that satisfies {!is_eol}. This can be
      [Sys.max_string s]. *)

  (** {1:uchar UTF-8 encoded Unicode characters} *)

  val utf_8_decode_len : char -> int
  (** [utf_8_decode_len b] is the length of an UTF-8 encoded Unicode
      character starting with byte [b]. This is [1] on UTF-8
      continuation or malformed bytes. *)

  val is_utf_8_decode : char -> bool
  (** [is_utf_8_decode c] is [true] iff [c] is not an UTF-8 continuation
      byte. This means [c] is either an UTF-8 start byte or an UTF-8
      malformed byte. *)

  val find_next_utf_8_decode : string -> start:int -> int
  (** [find_next_utf_8_sync s ~start] is either [Sys.max_string s] or the
      index of the byte at or after [start] that satisfies
      {!is_utf_8_decode}. *)

  val find_prev_utf_8_decode : string -> start:int -> int
  (** [find_prev_utf_8_decode s ~start] is either [0] or the index of the
      byte at or before [start] that satisfies {!is_utf_8_decode}. *)

  (** {1:white Whitespace} *)

  val is_white : char -> bool
  (** [is_white c] is [true] iff [c] is US-ASCII whitespace (0x20,
      0x09, 0x0A, 0x0B, 0x0C or 0x0D). *)

  val find_next_white : string -> start:int -> int
  (** [find_next_white s ~start] is either [String.length s] or the first
      byte position at or after [start] such that {!is_white} is
      [true]. *)

  val find_prev_white : string -> start:int -> int
  (** [find_prev_white s ~start] is either either [0] or the first byte
      position at or before [start] such that {!is_white} is
      [true]. *)

  (** {1:words Words} *)

  val find_next_after_eow : string -> start:int -> int
  (** [find_next_after_eow] is either [String.length s] or the byte position
      of the first {!is_white} after first skipping white and then
      non-white starting at [start]. *)

  val find_prev_sow : string -> start:int -> int
  (** [find_prev_sow] is either [0] or the byte position after skipping
      backward first white and then non-white. *)

  (** {1:gc Grapheme clusters and TTY width}

      {b Note.} This is a simple notion of grapheme cluster based
      on {!Uucp.Break.tty_width_hint}. *)

  val find_next_gc : string -> after:int -> int
  (** [find_next_gc s ~after] is [String.length s] or the byte position of
      the grapheme cluster after the one starting at [after]. *)

  val find_next_gc_and_tty_width : string -> after:int -> int * int
  (** [find_next_gc_and_width s ~after] is like {!find_next_gc} but
      also returns in the second component the tty width of the
      grapheme cluster at [after]. *)

  val find_prev_gc : string -> before:int -> int
  (** [find_prev_gc s ~before] is [0] or the the byte position of the
      grapheme cluster before the one starting at [before]. *)

  val find_prev_eol_and_tty_width : string -> before:int -> int * int
  (** [find_prev_eol_and_tty_width s ~before] is either [0] or the
      index of the byte before [before] that satisfies {!is_eol} and
      in the second component, the tty width needed to go from that index
      to [before]. *)

  val find_next_tty_width_or_eol : string -> start:int -> w:int -> int
  (** [find_next_tty_width_or_eol s ~start ~w] is the index of the grapheme
      cluster after TTY width [w] at or after [start] or of the next
      end of line if that happened before. *)
end

(** Text entries parsing.

    Parsing text made of entries separated by a special line. *)
module Txt_entries : sig
  val to_string : sep:string -> string list -> string
  (** [to_string ~sep es] converts entries [es] to a string
      by concatening them and separating them by lines containing
      [sep]. *)

  val of_string : sep:string -> string -> string list
  (** [of_string ~sep s] are the entries of [s]. Entries
      are separated by trimmed lines that contain [sep]. *)
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

  val exists : string -> (bool, string) result
  (** [exists dir] is [true] if [dir] exists as a directory. *)

  val contents : string -> (string list, string) result
  (** [contents dir] is the directory contents of [dir]. *)
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
  [ `Bold | `Faint | `Italic | `Underline | `Reverse
  | `Fg of [ color | `Hi of color] | `Bg of [ color | `Hi of color] ]
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

  val cursor_hide : string
  (** [cursor_hide] hides the cursor. *)

  val cursor_show : string
  (** [cursor_show] shows the cursor. *)

  val cursor_origin : string
  (** [cursor_origin] moves cursor to the top-left origin. *)

  val clear_screen : string
  (** [clear_screen] clears the screen. *)

  (** {1:input Input} *)

  type arrow = [ `Up | `Down | `Left | `Right ]
  type input =
  [ `Arrow of arrow | `Backspace | `Bytes of string
  | `Ctrl of [ `Key of int | `Arrow of arrow]
  | `Delete | `End | `Enter | `Escape | `Function of int
  | `Home | `Meta of int | `Page of [ `Up | `Down ]
  | `Shift of [`Arrow of arrow ] | `Tab | `Unknown of string ]
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

  val enable_bracketed_paste : out_channel -> unit
  (** [enable_bracketed_paste oc] enables
      {{:http://www.xfree86.org/4.7.0/ctlseqs.html#Bracketed%20Paste%20Mode}
      bracketed paste mode} on [oc]. This is disabled at exit on the
      same channel via an {!at_exit} handler. *)

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

  val pr : ('a, Format.formatter, unit) format -> 'a
  (** [pf] is {!Format.printf} *)

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

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** An OCaml toplevel (REPL) upgrade

    See the {{!page-manual}manual}. *)

(** {1 Down} *)

val help : unit -> unit
(** [help ()] prints help about Down. *)

(** Manage sessions.

    See the {{!page-manual.sessions}manual}. *)
module Session : sig

  (** {1:sessions Sessions} *)

  type name = string
  (** The type for session names. Use [""] to denote the {!last_name}
      session. *)

  val last_name : unit -> string option
  (** [last_name ()] is the last session name that was used with any
      of the functions of this module; if any and still
      existing. Persisted accross [ocaml] invocations. *)

  val list : unit -> unit
  (** [list ()] lists available sessions. *)

  val load : ?silent:bool -> name -> unit
  (** [load s] loads and executes session [s]. If [silent] is [true]
      the result of phrases is not printed out (defaults to
      [false]). *)

  val edit : name -> unit
  (** [edit s] edits session [s] in your editor. A session is created
      if [s] does not exist. *)

  val of_file : ?replace:bool -> file:string -> name -> unit
  (** [of_file ~replace ~file s] takes the contents of file [file] and
      stores it session [s]. The function errors if [s] exists; unless
      [replace] is [true] (defaults to [false]). *)

  val delete : name -> unit
  (** [delete s] deletes session [s]. *)

  (** {1:record Recording}

      {b Note.} Unsaved recorded phrases are persisted across [ocaml]
      sessions. *)

  val record : unit -> unit
  (** [record] starts recording phrases. *)

  val stop : unit -> unit
  (** [stop] stops recording phrases. *)

  val revise : unit -> unit
  (** [revise ()] edits recorded phrases. *)

  val save : ?replace:bool -> name -> unit
  (** [save s] saves recorded phrases to [s], stops recording and
      clears the recorded phrases. The function errors and the
      recorded phrases are kept intact if [s] exists; unless [replace]
      is [true] (defaults to [false]). See also {!append}. *)

  val append : name -> unit
  (** [append s] is like {!save} except it appends to [s] or creates
      it if it does not exist. *)

  (** {1:stepping Stepping} *)

  val steps : name -> unit
  (** [steps ()] loads a session for stepping through manually via
      [shift-{up,down}] (or [C-x C-{p,n}]). *)

  val next_step : unit -> unit
  (** [next_step ()] moves to the next step of the stepped session.
      Usually you do this via [shift-down] or [C-x C-n]. *)

  val prev_step : unit -> unit
  (** [prev_step ()] moves the previous step of the stepped session.
      Usually you do this via [shift-up] or [C-x C-p]. *)
end

(** Manage history. *)
module History : sig
  val edit : unit -> unit
  (** [edit ()] edits history in your editor. *)

  val clear : unit -> unit
  (** [clear ()] clears the history. *)
end

val tty_no_faint : unit -> unit
(** [tty_no_faint ()] disables uses of ANSI faint by down.  Some
    terminals switch to different colors which may be unpleasant, call
    this function if that is the case. *)

(** Private.

    Do not use. This is an unstable API subject to change even between
    minor versions of the library. *)
module Private : sig

  (** OCaml Toplevel API *)
  module type TOP = sig
    val read_interactive_input : (string -> bytes -> int -> int * bool) ref
    val use_file : Format.formatter -> string -> bool
    val use_silently : Format.formatter -> string -> bool
  end

  val set_top : (module TOP) -> unit
  (** [set_top t] sets the implementation of the OCaml toplevel to [t]. *)

  val unicode_version : string
  (** [unicode_version] is the Unicode version on which the
      {!Uucp.Break.tty_break_hint} data used by down is based. *)
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

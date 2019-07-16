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

(** Manage history. *)
module History : sig
  val edit : unit -> unit
  (** [edit ()] edits history in your editor. *)

  val clear : unit -> unit
  (** [clear ()] clears your history. *)
end

(** Manage sessions.

    See the {{!page-manual.sessions}manual}. *)
module Session : sig

  (** {1:sessions Sessions} *)

  type name = string
  (** The type for session names. Use [""] to denote the {!last}
      session. *)

  val last : unit -> string option
  (** [last ()] is the last session executed via {!load}; if any and still
      existing. *)

  val list : unit -> unit
  (** [list ()] lists available sessions. *)

  val load : ?silent:bool -> name -> unit
  (** [load n] loads and executes session [n]. If [silent] is [true]
      the result of phrases is not printed out (defaults to [false]). *)

  val edit : name -> unit
  (** [edit n] edits session [n] in your editor. A session is
      created if [n] doesn't exist. *)

  val of_file : ?replace:bool -> file:string -> name -> unit
  (** [of_file ~replace ~file n] takes the contents of file [file]
      and stores it session [n]. The function errors if [n] exists; unless
      [replace] is [true] (defaults to [false]). *)

  val delete : name -> unit
  (** [delete n] deletes session [n]. *)

  (** {1:record Recording sessions} *)

  val start : unit -> unit
  (** [start] starts recording phrases. *)

  val stop : unit -> unit
  (** [stop] stops recording phrases. *)

  val revise : unit -> unit
  (** [revise ()] edit recorded phrases. *)

  val save : ?replace:bool -> name -> unit
  (** [save n] saves recorded phrases, clears the recording buffer and
      stops recording. The function errors and the recording buffer is
      kept intact if [n] exists; unless [replace] is [true] (defaults
      to [false]). *)

  val append : name -> unit
  (** [append n] is like {!save} except it appends to [n] (and creates
      it if it doesn't exist). *)

  (** {1:stepping Stepping sessions} *)

  val steps : name -> unit
  (** [steps ()] loads a session for stepping through manually via
      [C-x C-p] and [C-x C-n]. *)
end

(** {1 Private} *)


(** Private API

    This is an unstable API subject to change even between minor versions
    of the library. Use at your own risk. *)
module Private : sig

  (** {1:down Down's private area} *)

  module type TOP = sig
    val readline : (string -> bytes -> int -> int * bool) ref
    val exec_phrase : print_result:bool -> string -> (bool, exn) result
    val use_file : Format.formatter -> string -> bool
    val use_silently : Format.formatter -> string -> bool
  end

  val set_top : (module TOP) -> unit
  (** [set_top t] sets the toplevel implementation to [t]. *)
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

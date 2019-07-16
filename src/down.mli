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

val exec_phrase : print_result:bool -> string -> (bool, exn) result

(** Manage your history. *)
module History : sig
  val edit : unit -> unit
  (** [edit ()] edits history in your editor. *)

  val clear : unit -> unit
  (** [clear ()] clears your history. *)
end

(** Manage sessions. *)
module Session : sig
(*

  val list : unit -> unit
  (** [list ()] lists sessions. *)

val copy : ?doc:string -> string -> string -> unit
val rename : ?doc:string -> string -> string -> unit
val delete : string -> unit

val load : ?replay:bool -> string -> unit
(** [load ~replay n] loads session [n] and if [replay] is [false] (default) *)

val save : ?doc:string -> string -> unit
(** [save ~append ~doc n] saves the current session under the name [n].
    If [n] already exists and [append] is [false] *)

(** {1:replay Replay commands}

    Replay can be interleaved with other commands.

    Cursor:

    {ul
    {- Exec point and navigation.}
    {- Need to be able to move both}} *)
*)
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

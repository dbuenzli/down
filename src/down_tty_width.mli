(*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TTY Unicode scalar value width. *)

val unicode_version : string
(** [unicode_version] is the Unicode version for which the width
    data is derived. *)

val of_utf_8 : string -> start:int -> int
(** [of_utf_8 s ~start] is the TTY width of the Unicode scalar
    starting at [start] as determined by {!Uucp.Break.tty_width_hint}. *)

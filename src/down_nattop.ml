(*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = if !Sys.interactive then (Down.Private.set_top (module Opttoploop))

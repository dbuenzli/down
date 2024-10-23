(*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let next_uchar u = if Uchar.equal u Uchar.max then None else Some (Uchar.succ u)
let utf_8_of_uchar u =
  let b = Buffer.create 4 in Buffer.add_utf_8_uchar b u; Buffer.contents b

let assert_map () =
  let rec loop = function
  | None -> ()
  | Some u ->
      let w = Down_tty_width.of_utf_8 (utf_8_of_uchar u) ~start:0 in
      let uucp_w = match Uucp.Break.tty_width_hint u with
      | -1 -> 0 | w -> w
      in
      if not (w = uucp_w)
      then
        (Printf.printf "FAIL: U+%04X w:%d uucp:%d\n%!"
           (Uchar.to_int u) w uucp_w; assert false);
      loop (next_uchar u)
  in
  loop (Some Uchar.min)

let () =
  assert_map ();
  Printf.printf "Test succeeded!\n%!"

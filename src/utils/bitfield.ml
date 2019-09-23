type t = int64 * int64

let max_idx = 127
let empty = 0L, 0L
let full = -1L, -1L
let bit idx =
  if idx < 0 || idx >= 128 then assert false;
  if idx < 64 then Int64.shift_left 1L idx, 0L
  else 0L, Int64.shift_left 1L (idx - 64)
let logor (a, b) (c, d) = (Int64.logor a c, Int64.logor b d)
let logand (a, b) (c, d) = (Int64.logand a c, Int64.logand b d)
let lognot (a, b) = (Int64.lognot a, Int64.lognot b)
let set idx t = logor t (bit idx)
let unset idx t = logand t (lognot (bit idx))
let is_set t idx = logand t (bit idx) <> (0L, 0L)

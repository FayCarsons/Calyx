type t =
  { filename : string
  ; line : int64
  ; col : int64
  }

let empty filename = { filename; line = 0L; col = 0L }

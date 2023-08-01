include Str

let replace_forward regexp ~templ ~string start =
  let pos = search_forward regexp string start in
  (* The matched substring *)
  let sub = matched_string string in
  (* The last position of the matched substring *)
  let last = match_end () in
  (* The substring replaced by the regexp *)
  let templ = global_replace regexp templ sub in
  ( Mdrp_string.sub string 0 pos
    ^ templ
    ^ Mdrp_string.sub string last (Mdrp_string.length string - last),
    pos )

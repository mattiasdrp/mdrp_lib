let lines file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line ci with
    | s -> aux_parse (s :: acc)
    | exception End_of_file ->
        close_in ci;
        List.rev acc
  in
  aux_parse []

let fold_lines ?(end_line = false) f acc file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line ci with
    | s -> aux_parse (f acc s)
    | exception End_of_file ->
        close_in ci;
        if end_line then f acc "" else acc
  in
  aux_parse acc

let parse ?(sep = ';') file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line ci with
    | s ->
        let l = String.split_on_char sep s in
        aux_parse (l :: acc)
    | exception End_of_file ->
        close_in ci;
        List.rev acc
  in
  aux_parse []

let fold_parse ?(sep = ';') f acc file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line ci with
    | s ->
        let l = String.split_on_char sep s in
        aux_parse (List.fold_left f acc l)
    | exception End_of_file ->
        close_in ci;
        acc
  in
  aux_parse acc

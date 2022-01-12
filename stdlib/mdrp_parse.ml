open Mdrp_input

let lines file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line_opt ci with
    | Some s -> aux_parse (s :: acc)
    | None ->
        close_in ci;
        List.rev acc
  in
  aux_parse []

let fold_lines f acc file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line_opt ci with
    | Some s -> aux_parse (f acc s)
    | None ->
        close_in ci;
        acc
  in
  aux_parse acc

let parse ?(sep = ';') file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line_opt ci with
    | Some s ->
        let l = String.split_on_char sep s in
        aux_parse (l :: acc)
    | None ->
        close_in ci;
        List.rev acc
  in
  aux_parse []

let fold_parse ?(sep = ';') f acc file =
  let ci = open_in file in

  let rec aux_parse acc =
    match input_line_opt ci with
    | Some s ->
        let l = String.split_on_char sep s in
        aux_parse (List.fold_left f acc l)
    | None ->
        close_in ci;
        acc
  in
  aux_parse acc

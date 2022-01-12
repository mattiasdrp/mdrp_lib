type 'a t = 'a list * 'a list

let empty = ([], [])

let push item (input, output) = (item :: input, output)

let is_empty = function [], [] -> true | _ -> false

let rec pop_opt = function
  | input, item :: output -> Some (item, (input, output))
  | [], [] -> None
  | input, [] -> pop_opt ([], List.rev input)

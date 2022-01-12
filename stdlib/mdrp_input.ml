let input_line_opt file = try Some (input_line file) with End_of_file -> None

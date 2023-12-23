let readl () = try Some (read_line ()) with End_of_file -> None

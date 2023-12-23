let read () = try Some (read_line ()) with End_of_file -> None

let solve = Day07.part1

let () = print_int (solve ()) ; print_endline ""

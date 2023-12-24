let readl () = try Some (read_line ()) with End_of_file -> None

let print_list f l =
  match l with
  | h :: l ->
      print_string "[ ";
      f h;
      List.iter
        (fun x ->
          print_string "; ";
          f x)
        l;
      print_string " ]"
  | _ -> print_string "[]"

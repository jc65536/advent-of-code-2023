open Aoc.Util

type dir = L | R

type node = {
  name : string;
  left : node Lazy.t;
  right : node Lazy.t;
  node_after_one_pass : node Lazy.t;
  z_offsets : int list Lazy.t;
  loop_info : loop_info Lazy.t;
}

(*
                Z <--- Z
                | loop ^
                v      |
  A ---> Z ---> C ---> D
  |----ramp-----|
*)
and loop_info = {
  ramp_passes : int;
  ramp_z_offsets : int list;
  loop_node : node;
  loop_passes : int;
  loop_z_offsets : int list;
}

let print_node node =
  Format.printf
    "{ name = %s; left = %s; right = %s; node_after_one_pass = %s; z_offsets = \
     %!"
    node.name (Lazy.force node.left).name (Lazy.force node.right).name
    (Lazy.force node.node_after_one_pass).name;
  print_list print_int (Lazy.force node.z_offsets);
  let info = Lazy.force node.loop_info in
  Format.printf "; loop_info = { ramp_passes = %d; ramp_z_offsets = %!"
    info.ramp_passes;
  print_list print_int info.ramp_z_offsets;
  Format.printf "; loop_node = %s; loop_passes = %d; loop_z_offsets = %!"
    info.loop_node.name info.loop_passes;
  print_list print_int info.loop_z_offsets;
  print_endline " } }"

let parse_instrs () =
  match readl () with
  | Some line ->
      String.fold_right
        (function 'L' -> Seq.cons L | _ -> Seq.cons R)
        line Seq.empty
  | _ ->
      print_endline "Input error";
      exit 1

let find_node name = List.find (fun n -> n.name = name)
let step d node = Lazy.force (match d with L -> node.left | R -> node.right)
let ends_with ch node = node.name.[2] == ch

let compute_pass_info instrs node =
  let node_after_one_pass, z_offsets =
    Seq.fold_lefti
      (fun (n, zs) i d -> (step d n, if ends_with 'Z' n then i :: zs else zs))
      (node, []) instrs
  in
  (node_after_one_pass, List.rev z_offsets)

let rec compute_loop_passes_and_offsets offsets hist hist_len node =
  if List.mem node hist then (hist_len, offsets)
  else
    let hist = node :: hist
    and offsets = if ends_with 'Z' node then hist_len :: offsets else offsets in
    compute_loop_passes_and_offsets offsets hist (hist_len + 1)
      (Lazy.force node.node_after_one_pass)

let compute_loop_info node =
  try
    let next_loop_info =
      Lazy.force (Lazy.force node.node_after_one_pass).loop_info
    in
    {
      ramp_passes = next_loop_info.ramp_passes + 1;
      ramp_z_offsets = List.map (( + ) 1) next_loop_info.ramp_z_offsets;
      loop_node = next_loop_info.loop_node;
      loop_passes = next_loop_info.loop_passes;
      loop_z_offsets = next_loop_info.loop_z_offsets;
    }
  with Lazy.Undefined ->
    let loop_passes, loop_z_offsets =
      compute_loop_passes_and_offsets [] [] 0 node
    in
    {
      ramp_passes = 0;
      ramp_z_offsets = [];
      loop_node = node;
      loop_passes;
      loop_z_offsets;
    }

let parse_node instrs nodes line =
  let name = String.sub line 0 3
  and left_name = String.sub line 7 3
  and right_name = String.sub line 12 3 in
  let rec node =
    {
      name;
      left = lazy (find_node left_name (Lazy.force nodes));
      right = lazy (find_node right_name (Lazy.force nodes));
      node_after_one_pass = lazy (fst (Lazy.force pass_info));
      z_offsets = lazy (snd (Lazy.force pass_info));
      loop_info = lazy (compute_loop_info node);
    }
  and pass_info = lazy (compute_pass_info instrs node) in
  node

let rec parse_graph instrs nodes =
  match readl () with
  | Some line -> parse_node instrs nodes line :: parse_graph instrs nodes
  | _ -> []

let parse_input () =
  let instrs = parse_instrs () in
  ignore (readl ());
  let rec graph = lazy (parse_graph instrs graph) in
  (instrs, Lazy.force graph)

let rec traverse (instrs : dir Seq.t) steps node =
  if node.name = "ZZZ" then steps
  else
    match instrs () with
    | Cons (d, instrs) -> traverse instrs (steps + 1) (step d node)
    | _ ->
        print_endline "Instructions terminated";
        exit 1

let rec pop_lists heads tails = function
  | [] -> Some (heads, tails)
  | (x :: xs) :: ls -> pop_lists (x :: heads) (xs :: tails) ls
  | _ -> None

let pop_lists_lt_max ls =
  Option.map
    (function
      | h :: hs, tails ->
          let max_head = List.fold_left Int.max h hs in
          let pop_if_lt_max h t = if h < max_head then t else h :: t in
          List.rev_map2 pop_if_lt_max (h :: hs) tails
      | _ -> [])
    (pop_lists [] [] ls)

let heads_all_eq ls =
  let rec _heads_all_eq ref = function
    | [] -> Some true
    | (h :: _) :: ls -> (
        match ref with
        | None -> _heads_all_eq (Some h) ls
        | Some ref ->
            if h = ref then _heads_all_eq (Some ref) ls else Some false)
    | _ -> None
  in
  _heads_all_eq None ls

let rec steps_to_term_in_one_pass nodes_z_offsets =
  Option.bind (pop_lists_lt_max nodes_z_offsets) (fun nzo ->
      println_list (print_list print_int) nzo;
      Option.bind (heads_all_eq nzo) (fun eq ->
          if eq then match nzo with (h :: _) :: _ -> Some h | _ -> None
          else steps_to_term_in_one_pass nzo))

let rec traverse2 steps nodes =
  match
    steps_to_term_in_one_pass
      (List.map (fun node -> Lazy.force node.z_offsets) nodes)
  with
  | Some rem -> (steps, rem)
  | None ->
      traverse2 (steps + 1)
        (List.map (fun node -> Lazy.force node.node_after_one_pass) nodes)

let part1 () =
  let instrs, graph = parse_input () in
  traverse (Seq.cycle instrs) 0 (find_node "AAA" graph)

let cheat pass_len nodes =
  println_list print_node nodes;
  let info_list = List.map (fun node -> Lazy.force node.loop_info) nodes in
  let loop_passes_list = List.map (fun i -> i.loop_passes) info_list in
  let factors = List.map Z.of_int loop_passes_list in
  let gcd = List.fold_left Z.mul (List.hd factors) (List.tl factors) in
  Z.mul gcd (Z.of_int pass_len)

let part2 () =
  let instrs, graph = parse_input () in
  Z.print (cheat (Seq.length instrs) (List.filter (ends_with 'A') graph));
  print_endline "";
  0

open Graph
open Bfs_smt
open Dfs_smt
open Bfs_subst
open Dfs_subst

open Full_smt

let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx

let rec mainloop () = 
Printf.printf "Enter a filename: \n";
let filename = read_line() in
let graph = mk_graph_from_file filename in
let terminal_node_idxs = get_terminal_node_idx_list graph in
let input_alph = get_input_alph graph in
Printf.printf "Enter a search depth: \n";
let depth = int_of_string (read_line()) in
Printf.printf "Running full SMT search... \n";
time (init_smt_search graph depth) (List.hd terminal_node_idxs);
(* Printf.printf "Running partial SMT search... \n";
time (init_smt_reverse_bfs graph depth) (List.hd terminal_node_idxs);
Printf.printf "Running substitution BFS search... \n";
time (init_subst_reverse_bfs graph input_alph depth) (List.hd terminal_node_idxs); *)
Printf.printf "Exiting... \n";
exit 0
(*mainloop ()*)

let () = mainloop ()
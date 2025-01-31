open Leq

let memory_size = 2048 (* Define a reasonable memory size *)

let main () =
  let args = Sys.argv |> Array.to_list |> List.tl in (* Get command-line arguments, skipping program name *)
  match args with
  | ["assemble"; infile; outfile] ->
      let source_text = In_channel.with_open_bin infile In_channel.input_all in
      let assembled_memory = assemble source_text in (* Call the assemble function! *)
      let assembled_state = { memory = assembled_memory; pc = 0 } in (* Create a state *)
      Out_channel.with_open_bin outfile (fun oc -> dump_triples assembled_state oc) (* Write to outfile *)

  | ["run"; infile] ->
      let state = create_state memory_size in
      In_channel.with_open_bin infile (load_triples state);
      run state ~trace:false

  | ["trace"; infile] ->
      let state = create_state memory_size in
      In_channel.with_open_bin infile (load_triples state);
      run state ~trace:true

  | _ ->
      Printf.eprintf "usage: %s assemble in.subleqasm out.triples | run in.triples | trace in.triples\n" Sys.argv.(0);
      exit 1

let () = main ()

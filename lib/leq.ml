open Hashtbl

type subleq_state = {
  mutable memory : int array;
  mutable pc : int;
}

(* Create State *)

let create_state memory_size = {
  memory = Array.make memory_size 0;
  pc = 0;
}

exception ParseError of string

(* Load Triples *)

let load_triples state input_channel =
  let memory_list = ref [] in
  try
    while true do
      let line = input_line input_channel in
      let parts = String.split_on_char ' ' line in
      let a = int_of_string (List.nth parts 0) in
      let b = int_of_string (List.nth parts 1) in
      let c = int_of_string (List.nth parts 2) in
      memory_list := a :: b :: c :: !memory_list (* Add in reverse order *)
    done
  with
  | End_of_file -> ()
  | _ -> (raise (ParseError "Error parsing input triples") : unit)
  ;
  let memory_array = Array.of_list (List.rev !memory_list) in
  state.memory <- memory_array

(* State *)

let run state ~trace =
  state.pc <- 0; (* Reset program counter *)
  while state.pc >= 0 do
    if state.pc >= Array.length state.memory - 2 then (* Check for program counter out of bounds *)
      failwith "Program counter out of bounds";
    let a = state.memory.(state.pc) in
    let b = state.memory.(state.pc + 1) in
    let c = state.memory.(state.pc + 2) in
    if trace then
      Printf.printf "pc=%d, a=%d, b=%d, c=%d\n" state.pc a b c;

    let at_a = state.memory.(a) in
    let result =
      if b >= 0 then
        let at_b = state.memory.(b) in
        let new_value_b = at_b - at_a in
        state.memory.(b) <- new_value_b;
        new_value_b
      else if b = -1 then (
        output_char stdout (Char.chr at_a);
        0 (* result is 0 for output *)
      ) else
        0 (* Handle other cases of b if needed, for now treat as no-op *)
    in

    if result <= 0 then
      state.pc <- c
    else
      state.pc <- state.pc + 3
  done

(* Dump Triples *)

let dump_triples state output_channel =
  let len = Array.length state.memory in
  let addr = ref 0 in
  while !addr < len do
    let a = if !addr < len then state.memory.(!addr) else 0 in
    let b = if !addr + 1 < len then state.memory.(!addr + 1) else 0 in
    let c = if !addr + 2 < len then state.memory.(!addr + 2) else 0 in
    Printf.fprintf output_channel "%d %d %d\n" a b c;
    addr := !addr + 3
  done

(* Collect Labels *)

let collect_labels source_text =
  let labels = create 100 in (* Initialize a hash table for labels *)
  let label_it s addr =
    if String.contains s ':' then
      match String.split_on_char ':' s with
      | [label; _contents] -> add labels label addr
      | _ -> () (* Ignore if split doesn't produce two parts *)
  in
  let addr = ref 0 in
  String.split_on_char '\n' source_text
  |> List.iter (fun line ->
    let line = String.trim line in
    if line <> "" && not (String.starts_with ~prefix:"#" line) then (
      match String.split_on_char ' ' line with
      | [a; b; c] ->
          label_it a !addr;
          label_it b (!addr + 1);
          label_it c (!addr + 2);
          addr := !addr + 3
      | _ -> () (* Ignore lines that don't have 3 parts *)
    )
  );
  labels

(* Write Contents *)

let write_contents source_text labels =
  let memory_contents = ref [] in
  let write_it v addr =
    let v =
      if String.contains v ':' then
        match String.split_on_char ':' v with
        | [_; contents] -> contents (* Take content after label *)
        | _ -> v (* Keep original if split fails *)
      else v
    in
    let v =
      if mem labels v then
        string_of_int (find labels v)
      else if v = "?+1" then
        string_of_int (addr + 3)
      else v
    in
    try
      int_of_string v
    with
    | _ -> failwith ("Invalid value in assembly source: " ^ v)
  in
  let addr = ref 0 in
  String.split_on_char '\n' source_text
  |> List.iter (fun line ->
    let line = String.trim line in
    if line <> "" && not (String.starts_with ~prefix:"#" line) then (
      match String.split_on_char ' ' line with
      | [a; b; c] ->
          memory_contents := (write_it a !addr) :: !memory_contents;
          memory_contents := (write_it b !addr) :: !memory_contents;
          memory_contents := (write_it c !addr) :: !memory_contents;
          addr := !addr + 3
      | _ -> () (* Ignore lines that don't have 3 parts *)
    )
  );
  List.rev !memory_contents (* Reverse to get correct order *)

(* Assemble *)

let assemble source_text =
  let labels = collect_labels source_text in
  let contents_list = write_contents source_text labels in
  Array.of_list contents_list



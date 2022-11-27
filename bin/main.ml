
let () =
	print_endline "";
	for i=1 to 8 do
		let filename = "input" ^ (string_of_int i) ^ ".txt" in
		print_endline ("Executing: " ^ filename ^ "....");
		let _ = try Brainfuck_ocaml_lib.run_program filename with
			Failure e -> print_string ("Exception occured: " ^ e)
		in
		print_endline ("\n")
	done;

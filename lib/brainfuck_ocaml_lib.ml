
let nUM_OF_CELLS = 30

module Str = struct
	include Str
	let search_forward_exn regex str start = 
		try Some (Str.search_forward regex str start) with
		 Not_found -> None
end

let input_uint8 () =
	let line = read_line () in
	let byte_opt = int_of_string_opt line in
	if byte_opt = None then
		failwith "INCORRECT VALUE"
	else
		let byte = Option.get byte_opt in
		if byte < 0 || byte > 255 then
			failwith "INCORRECT VALUE"
		else
			byte

(******************************************************)

let parse cells token ip dp braces_ix_list =
	match token with
	| '>' -> 
		if dp = nUM_OF_CELLS then
			failwith "POINTER_OUT_OF_BOUNDS"
		else
			(ip+1), (dp+1)
	| '<' -> 
		if dp = 0 then
			failwith "POINTER_OUT_OF_BOUNDS"
		else
			(ip+1), (dp-1)
	| '+' -> 
		if cells.(dp) = 255 then
			failwith "INCORRECT_VALUE"
		else
			cells.(dp) <- (cells.(dp) + 1); (* cells.set dp ((cells.get dp) + 1) *)
			(ip+1), dp
	| '-' ->
		if cells.(dp) = 0 then
			failwith "INCORRECT_VALUE"
		else 
			cells.(dp) <- (cells.(dp) - 1); 
			(ip+1), dp
	| '.' -> 
		print_char (char_of_int cells.(dp));
		(ip+1), dp
	| ',' -> 
		print_string "from 0 to 255> ";
		cells.(dp) <- input_uint8 ();
		(ip+1), dp
	| '[' -> 
		if cells.(dp) = 0 then
			(* Find square-braces pair to get matching closing brace index *)
			let _,close_brace_ix = List.find (fun (open_brace_ix,_) -> open_brace_ix = ip) braces_ix_list in
			(close_brace_ix+1, dp)
		else
			(ip+1, dp)
	| ']' -> 
		if cells.(dp) != 0 then
			(* Find square-braces pair to get matching open brace index *)
			let open_brace_ix,_ = List.find (fun (_,close_brace_ix) -> close_brace_ix = ip) braces_ix_list in
			(open_brace_ix+1, dp)
		else
			(ip+1, dp)
	| c -> 
		failwith ("Trying to parse unknown character: " ^ (String.make 1 c))

(********************************************************************)

(* 1. Read program from file *)
let get_program file =
	let rec build_list ic lines =
        match input_line ic with
        | exception End_of_file -> lines
        | line -> build_list ic (line::lines)
    in
    let ic = open_in file in
    let result = build_list ic [] in
    List.rev result

(* 2. Clean up all non-code characters (Whitespace and comments) *)
let clean_up_lines lines =
	let rec inner2 lines_left clean_lines count =
		if count > 0 then
			let line = List.hd lines_left in
			let _ = try Str.search_forward (Str.regexp "[]+<>,.\\[-]+") line 0 with
			 Not_found -> failwith "Err: clean_up_lines"
			in
			let clean_line = Str.matched_string line in
			inner2 (List.tl lines_left) (clean_line::clean_lines) (count-1)
		else
			clean_lines
	in
	inner2 lines [] (List.length lines)

(* 3. Concatenate all lines of the program to a single string. 
      (No seperate function to perform this action) *)

(* 4. Syntax check for matching square braces (a starting '[' needs to have a matching ']') *)
let check_square_braces str =
	(* Store each '[' that we find and
	   pop each time we find a ']' *)
	let open_braces_ix = Stack.create () in

	(* Recursive function that finds the next square-brace on each call *)
	let rec find_next_brace start braces_ix_list =
		(* Regex to find either a '[' or ']' *)
		match Str.search_forward_exn (Str.regexp "\\(]\\|\\[\\)") str start with
		| None ->
			if Stack.is_empty open_braces_ix then
				braces_ix_list
			else
				(* We found no more closing braces when we have open braces left to match *)
				failwith "SYNTAX_ERROR"
		| Some ix ->
			let brace = String.get str ix in
			if brace = '[' then
				let () = Stack.push ix open_braces_ix in
				find_next_brace (ix+1) braces_ix_list
			else (* brace = ']' *)
				if (Stack.is_empty open_braces_ix) || ix = (String.length str) then
					(* We find closing brace with no open brace to match with OR
					   We find closing brace that is last character of program *)
					failwith "SYNTAX_ERROR"
				else
					let open_brace_ix = Stack.pop open_braces_ix in
					find_next_brace (ix+1) ((open_brace_ix, ix)::braces_ix_list)
	in
	find_next_brace 0 []

(* 5. Execute the brainfuck code-string *)
let execute_code prog braces_ix_list =
	let ip = ref 0 in
	let dp = ref 0 in
	let cells = Array.make nUM_OF_CELLS 0 in
	while !ip < (String.length prog) do
		let token = String.get prog !ip in
		let c_ip,c_dp = parse cells token !ip !dp braces_ix_list in
		ip := c_ip;
		dp := c_dp;
		()
	done;
	()

(******************************************************************)
(*                     PUBLIC FUNCTIONS (etc.)                    *)
(******************************************************************)

let run_program filename = 
	(* 1. Read out program from file and put it into a list of strings (each element is a line) *)
	let prog_lines = get_program filename in

	(* 2. Clean up all leading/trailing non program characters (Code-comments, whitespace etc.) *)
	let clean_lines = clean_up_lines prog_lines in

	(* 3. Concatenate all the lines so program becomes a single string *)
	let clean_prog_str = List.fold_right (fun a b -> b ^ a) clean_lines "" in

	(* 4. Syntax check for matching square braces (a starting '[' needs to have a matching ']') *)
	let braces_ix_list = check_square_braces clean_prog_str in

	(* 5. Execute the brainfuck code-string *)
	execute_code clean_prog_str braces_ix_list

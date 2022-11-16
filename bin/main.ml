
let nUM_OF_CELLS = 30
let cells = Array.make nUM_OF_CELLS 0

(* Modulo functions that handles subtraction wrap-around *)
let modulo a b = 
	let res = a mod b in
	if res >= 0 then res
	else res + b


module String = struct
	include String
	(* val find_ix : string -> char -> int option 

	   Returns index of first char 'c' found in string 's', 
	   starting from 0 or None if no such char is found in string *)
	(*let find_ix s c = 
		match Base.String.findi ~f:(fun i c -> c = '[') with
		| None -> None
		| Some res -> let ix,_ = Option.get res in ix
	in*)

	(* val lsplit2_index : string -> char -> (int * string) option *)
	(*let lsplit2_index s c =
		let ix = Base.String.index str '[' in
		let 
	in*)
	(* Substring tail - Get the substring starting from 'pos' and whats trailing after *)
	let sub_tl str pos = 
		String.sub str pos ((String.length str)-pos-1)
end

module Str = struct
	include Str
	let search_forward_exn regex str start = 
		try Some (Str.search_forward regex str start) with
		 Not_found -> None
end

(******************************************************)

let getc () =
	let line = input_line stdin in
	let byte_opt = int_of_string_opt line in
	if byte_opt = None then
		print_endline "INCORRECT VALUE"
	else
		let byte = Option.get byte_opt in
		if byte < 0 || byte > 255 then
			print_endline "INCORRECT VALUE"
		else
			()

let parse token ip dp braces_ix_list =
	match token with
	| '>' -> 
		(ip+1), modulo (dp+1) nUM_OF_CELLS
	| '<' -> 
		(ip+1), modulo (dp-1) nUM_OF_CELLS
	| '+' -> 
		cells.(dp) <- (cells.(dp) + 1); (* cells.set dp ((cells.get dp) + 1) *)
		(ip+1), dp
	| '-' -> 
		cells.(dp) <- (cells.(dp) - 1); 
		(ip+1), dp
	| '.' -> 
		(*print_endline (string_of_int cells.(dp));*)
		print_char (char_of_int cells.(dp));
		(ip+1), dp
	| ',' -> 
		getc ();
		(ip+1),dp
	| '[' -> 
		if cells.(dp) = 0 then
			(* Find square-braces pair to get matching closing brace index *)
			let _,close_brace_ix = List.find (fun (open_brace_ix,_) -> open_brace_ix = ip) braces_ix_list in
			(close_brace_ix+1, dp)
			(*(1,dp)*)
		else
			(ip+1, dp)
	| ']' -> 
		if cells.(dp) != 0 then
			(* Find square-braces pair to get matching open brace index *)
			let open_brace_ix,_ = List.find (fun (_,close_brace_ix) -> close_brace_ix = ip) braces_ix_list in
			(open_brace_ix+1, dp)
			(*(1,dp)*)
		else
			(ip+1, dp)
	| c -> 
		print_endline ("parse unknown token " ^ (Base.Char.to_string c));
		(ip+1, dp)

let clean_up_lines lines =
	let rec inner2 lines_left clean_lines count =
		if count > 0 then
			let line = List.hd lines_left in
			let _ = print_endline ("string to match: " ^ line) in
			let _ = try Str.search_forward (Str.regexp "[]+<>,.\\[-]+") line 0 with
			 Not_found -> print_endline "error clean up lines"; 0
			in
			let clean_line = Str.matched_string line in
			let _ = print_endline clean_line in
			inner2 (List.tl lines_left) (clean_line::clean_lines) (count-1)
		else
			clean_lines
	in
	inner2 lines [] (List.length lines)

(* Read program from file *)
let get_program file =
	let rec build_list ic lines =
        match input_line ic with
        | exception End_of_file -> lines
        | line -> build_list ic (line::lines)
    in
    let ic = open_in file in
    let result = build_list ic [] in
    List.rev result

(* For codinggame.com unittests *)
(*let get_program2 () =
	let lines = [] in
	let l, s, n = Scanf.sscanf (input_line stdin) " %d  %d  %d" (fun l s n -> (l, s, n)) in
	for i = 0 to l - 1 do
	    let lines = (input_line stdin) :: lines in
	    ();
	done;
	List.rev lines
*)

(*
let rec check_square_braces str =
	let res = Base.String.lsplit2 str ~on:'[' in
	if res = None then
		(* No more open braces found (including if input string is empty). Just return *)
		()
	else
		let _,after_open = Option.get res in
		print_endline ("csb: " ^ after_open);
		let res = Base.String.lsplit2 after_open ~on:']' in
		if res = None then
			(* No matching close brace found *)
			print_endline "SYNTAX_ERROR"
		else
			check_square_braces after_open

(* EDIT: Won't really work for creating matching braces, in case of nested braces. *)
let rec check_square_braces2 str braces_ix_list =
	let ix_open = Base.String.index str '[' in
	if ix_open = None then
		(* No more open braces found (including if input string is empty). Just return *)
		()
	else
		let after_open = String.sub str (ix_open+1) ((String.length after_open)-ix_open-1) in
		let ix_close = Base.String.index after_open ']' in
		if ix_close = None then
			(* No matching close brace found *)
			print_endline "SYNTAX_ERROR"
		else
			check_square_braces after_open ((ix_open,ix_close)::braces_ix_list)
*)

let check_square_braces3 str =
	let open_braces_ix = Stack.create () in
	let rec next_brace chars_passed rem_str braces_ix_list =
		let rem_str_ix_opt = Str.search_forward_exn (Str.regexp "\\(]\\|\\[\\)") rem_str 0 in
		if rem_str_ix_opt = None then
			let _ = print_endline "no braces found by regex" in
			if Stack.is_empty open_braces_ix then
				braces_ix_list
			else
				(* We find no more braces when we have open braces left to match *)
				let _ = print_endline "SYNTAX_ERROR" in
				braces_ix_list
		else
			let rem_str_ix = Option.get rem_str_ix_opt in
			let ix = chars_passed + rem_str_ix in
			let brace = (String.get rem_str) ix in
			let _ = print_endline ("Found brace: " ^ (String.make 1 brace) ^ " at " ^ (string_of_int ix)) in
			if brace = '[' then
				let rem_str = String.sub_tl rem_str (ix+1) in
				let () = Stack.push ix open_braces_ix in
				let _ = print_endline ("rem_str" ^ rem_str) in
				next_brace ix rem_str braces_ix_list
			else (* brace = ']' *)
				if (Stack.is_empty open_braces_ix) || ix = (String.length rem_str) then
					(* We find closing brace with no open brace to match with OR
					   We find closing brace that is last character of program *)
					let _ = print_endline "SYNTAX_ERROR" in
					braces_ix_list
				else
					let rem_str = String.sub_tl rem_str (ix+1) in
					let open_brace_ix = Stack.pop open_braces_ix in
					let bla = (open_brace_ix, (ix+1)) in
					let _ = print_endline ("rem_str" ^ rem_str) in
					next_brace ix rem_str (bla::braces_ix_list)
	in
	next_brace 0 str []

(*
let check_square_braces2 =
	let open_brace = Base.String.find clean_prog_str (fun tok -> tok = '[') in
	if open_brace = None then
		()
	else
		Base.String.find 
*)

(*let print_cells () =
	for i = 0 to nUM_OF_CELLS-1 do
		print_int cells.(i);
		print_char ','
	done;
	print_endline "";
	()*)

let run_program prog braces_ix_list =
	let ip = ref 0 in
	let dp = ref 0 in
	while !ip < (String.length prog) do
		let token = String.get prog !ip in
		let c_ip,c_dp = parse token !ip !dp braces_ix_list in
		(*print_endline ("ip: " ^ (string_of_int !ip) ^ " tok: " ^ (String.make 1 token) ^ " dp: " ^ (string_of_int !dp));
		*)(*print_char token;
		print_endline "";*)
		(*print_cells ();*)
		ip := c_ip;
		dp := c_dp;
		()
	done;
	()

let () =
	(* Read out program from file and put it into a list of strings (each element is a line) *)
	let prog_lines = get_program "input.txt" in

	(* Clean up all leading/trailing non program characters (Code-comments, whitespace etc.) *)
	let clean_lines = clean_up_lines prog_lines in

	(* Concatenate all the lines so program becomes a single string *)
	let clean_prog_str = List.fold_right (fun a b -> print_endline ("a: " ^ a ^ " b: " ^ b);b ^ a) clean_lines "" in

	print_endline clean_prog_str;

	(* Syntax check for matching square braces (a starting '[' needs to have a matching ']') *)
	(*let () = check_square_braces clean_prog_str in*)

	(* Syntax check for matching square braces (a starting '[' needs to have a matching ']') 
       AND save the "instructor pointer" position for each braces pair *)
	(*let braces_ix_list = check_square_braces clean_prog_str [] in*)

	(* Syntax check for matching square braces (a starting '[' needs to have a matching ']') *)
	let braces_ix_list = check_square_braces3 clean_prog_str in

	print_endline "braces_ix_list length:";
	print_int (List.length braces_ix_list);
	print_endline "";
	for i = 0 to (List.length braces_ix_list)-1 do
		let openb,closeb = List.nth braces_ix_list i in
		print_int openb;
		print_char ',';
		print_int closeb;
		print_endline "";
		()
	done;

	run_program clean_prog_str braces_ix_list

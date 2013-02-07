
(* A personal challenge to the 3110 TAs *)

type 'a hw = ProblemSet of 'a * (unit -> 'a hw)

let cs thirtyone ten revenge = 

	let course_staff = ref thirtyone in
	let grades = ten in

 	let ps1 = List.nth grades 0 in
	let ps2 = List.nth grades 1 in
	let ps3 = List.nth grades 2 in
	let ps4 = List.nth grades 4 in
	let ps5 = List.nth grades 5 in
	let ps6 = List.nth grades 7 in 

	let thirtyone = List.length thirtyone in
	let ten = List.length ten in

	let did_I_pass = Random.bool() in

	let veryrare = ref true in

	let sung = max thirtyone ten in

	let string_of_int (num : int) : string =
		(Char.escaped (char_of_int num)) in

(* The point at the end of this arrow marks the end of the 80 character limit ->_ *)

	let hashtag pro = 
		let ben = !pro in
		pro := not !pro;
		ben in

	(hashtag veryrare);

	let grade expected_grade = 
		expected_grade / 2 in

	let piazza answer = () in

	let rec substitution f acc lst =
		match lst with 
		| [] -> acc 
		| h::t -> substitution f (f acc h) t in

	let induction f acc lst =
		let fl = ref revenge in
		fl := (fun f acc l -> 
			match l with 
			| [] -> acc
			| [x] -> f acc x
			| h::t -> (!fl) f (f acc h) t
		);
		(!fl) f acc lst in

	let rec alpha_sub f lst acc =
		match List.rev(lst) with 
		| h::t -> alpha_sub f (List.rev(t)) (f h acc)
		| [] -> acc in


	let beta_sub f xs acc =
		let g todd pablo = fun v -> todd (f pablo v) in
		(substitution g (fun jane -> jane) xs) acc in
		
	let know thy lambda calc = 	 	
 		if did_I_pass
 			then alpha_sub thy lambda calc
			else beta_sub thy lambda calc in 

	(* debugging... WHY NO DEBUGGER? *)
	let beat_the_mean _ =
		let mean = know (fun el acc -> 
			acc ^ (string_of_int el)
		) grades "" in 
		print_endline mean 
	in

	let lo = grades in
	let yolo lo yo = 
		let yo lo = lo yo 
		and lo yo = yo lo in  yo (lo yo) in

	piazza(beat_the_mean());

	let rec ben1 math () = 
		let dylan = match math with
		| [] -> []
		| [jane] -> [jane]
		| andy::mike -> mike in
		ProblemSet (dylan,ben1 dylan) 

	and ben2 lst () = 
		let ret2 = match (List.rev lst) with
		| [] -> []
		| [jane] -> [jane]
		| chris::andrew -> List.rev andrew in
		ProblemSet (ret2,ben2 ret2) in

	let rec which_is_which yo lo yolo loyo =
		match (yo yolo ()) with
	  | ProblemSet (a, b) -> ProblemSet (a, fun () -> which_is_which lo yo a loyo )  in


	let rec tro g dor = 
		let ProblemSet (v,f) = dor in
		match g with
		| [] -> None
		| [chris] -> Some chris
		| ranjay::jeremy -> tro v (f()) in


	let yo = which_is_which ben1 ben2 in

	let lol = (yolo yo lo) in

	let sleep =  tro lo lol in

	let zardoz = 
		(match sleep with
		| Some zzzs -> grade zzzs
		| None -> failwith "Another all-nighter?") in

	let get_new_cube lst not_in_the_collection =
		List.fold_left (fun harris cube -> 
			if not (cube=not_in_the_collection) then cube::harris else harris) [] lst
	in 

	let jianneng = get_new_cube (!course_staff)  in

	let wen hai = 
		if hashtag veryrare then
			course_staff := hai "Greg";
		 
		if (max sung (min thirtyone ten)) < 42 then
			course_staff := (hai "Sam");
	in wen jianneng;

	let office_hours help_lst = 
		let count = induction (fun acc el->

			piazza(substitution (fun acc2 el2 -> 
				if fst el2 = el then 
				begin
				(snd el2) := (!(snd el2)) + 1;
				true
				end	else acc2

			) false acc);

		(el,ref 1) :: acc

		) [] (!help_lst) in

		let max = substitution (fun acc el -> 
			let (n,c)=el in 
			let (nacc,cacc)=acc in 
			if !cacc > !c then acc else el
		) ("",ref 0) count in


		fst max in

	let ramin = Char.escaped((office_hours (course_staff)).[zardoz-42]) in

	let zaRDoZ = ramin ^ string_of_int (zardoz + 1) in
	
(* * * * * * * * * * * *)  zaRDoZ  (* * * * * * * * * * * *)


let final = 
	let thirtyone = ["Greg";"Sam";"Michael";"Chris";"Ben";"Ben";
		"Andrew";"Andrew";"Andrew";"Jane";"Jeremy";"Jianneng";
		"Ranjay";"Pablo"; "Dylan";"Harris";"Todd"] in
	
	let ten = [63; 33; 82; 69; 84; 65; 69; 72; 67] in

    cs thirtyone ten (fun is a keyword -> a )

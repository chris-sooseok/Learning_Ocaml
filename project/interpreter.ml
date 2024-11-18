

type stackValue = STRING of string | INT of int | BOOL of bool
| ERROR | NAME of string | UNIT

(* ------------------------- doesn't matter if the types are all capitalized? *)
type command = ADD | SUB | MUL | DIV | REM | NEG | PUSH of stackValue | POP
| SWAP | ToString | Println | QUIT | CAT | AND | OR | NOT | EQUAL | LESSTHAN
| BIND | IF | LET | END


let interpreter ((input : string), (output : string )) : unit = 

  let ic = open_in input in

  let oc = open_out output in

let rec loop_read acc =
  try
    let l = String.trim(input_line ic) in loop_read (l::acc)
  with
    | End_of_file -> List.rev acc in

let file_write sv = Printf.fprintf oc "%s\n" sv 

in

(* Generating a list of string command by reading input file line by line *)
let string_cmd_list = loop_read [] in

(*List.iter (fun i -> Printf.printf "%s\n" i) string_cmd_list *)

(* helper function for translating push str cmd to cmd *)
let push_convert_helper (push_cm : string) =

  (* retrieve stack value from str cmd to evaluate the stack value *)
  (* ------------------------- need to check error case? *)
  let sv = String.sub push_cm 5 (String.length push_cm - 5) in

  (* a function that checks if stack value is int *)
  let convertible_to_int sv = match int_of_string_opt sv with | Some _ -> true | None -> false in

  (* a function that checks if stack value is string *)
  let convertible_to_str sv = if String.get sv 0 = '"' && String.get sv (String.length sv - 1) = '"' then true else false in
    
  (* evaluate stack value and if it is int, convert to int *)
  if convertible_to_int sv then PUSH (INT(int_of_string(sv)))
  
  (* evaluate stack value and if it is str, convert to str *)
  else if convertible_to_str sv then PUSH (STRING(sv))

  (* if stack is neither int nor str, check for other types *)
  else
  match sv with
  | ":true:" -> PUSH (BOOL(true))
  | ":false:" -> PUSH (BOOL(false ))
  | ":error:" -> PUSH (ERROR)
  | ":unit:" -> PUSH (UNIT)
  | _ -> PUSH (NAME(sv))
  
in

(* Convert the list of string command into command list line by line from string_cmd_list *)
let rec convert_strCmd_to_svCmd str_cmd_list = 
  match str_cmd_list with
  | [] -> []
  | "add" :: tl -> ADD :: convert_strCmd_to_svCmd tl
  | "sub" :: tl -> SUB:: convert_strCmd_to_svCmd tl
  | "mul" :: tl -> MUL:: convert_strCmd_to_svCmd tl
  | "div" :: tl -> DIV:: convert_strCmd_to_svCmd tl
  | "rem" :: tl -> REM:: convert_strCmd_to_svCmd tl
  | "neg" :: tl -> NEG:: convert_strCmd_to_svCmd tl
  | "swap" :: tl -> SWAP:: convert_strCmd_to_svCmd tl
  | "toString" :: tl -> ToString:: convert_strCmd_to_svCmd tl
  | "println" :: tl -> Println:: convert_strCmd_to_svCmd tl
  | "quit" :: tl -> QUIT :: convert_strCmd_to_svCmd tl
  | "pop" :: tl -> POP :: convert_strCmd_to_svCmd tl
  | push_cm :: tl -> push_convert_helper push_cm :: convert_strCmd_to_svCmd tl

in

(* let sv_cmd_list : command list = convert_strCmd_to_svCmd string_cmd_list;; *)

(* Convert string command list to implemented typed command list *)
let sv_cmd_list : command list = convert_strCmd_to_svCmd string_cmd_list

in

let write_to_file sv processor_func cm_tl st_tl m: unit =
  file_write sv;
  processor_func cm_tl st_tl m
in

let quotation_filter str = STRING (String.sub str 1 (String.length str - 2))
  
in

let rec processor (sv_cmd_li: command list) (s : stackValue list) (m : (stackValue * stackValue) list list) : unit =
match (sv_cmd_li, s, m) with
| (QUIT :: cm_tl, s, m) -> ()
(* whether int is positive or negative, done through this line *)
| (PUSH INT(sv) :: cm_tl, s, m) -> processor cm_tl (INT(sv) :: s) m
  (* from the input file string is enclosed with "". Then, output doesn't contain "" *)
| (PUSH STRING(sv) :: cm_tl, s, m) -> processor cm_tl (STRING(sv) :: s) m
  (* -------------- any error case? *)
| (PUSH NAME(sv) :: cm_tl, s, m)-> processor cm_tl (NAME(sv) :: s) m
| (PUSH BOOL(sv) :: cm_tl, s, m) -> processor cm_tl (BOOL(sv) :: s) m
| (PUSH ERROR :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (PUSH UNIT :: cm_tl, s, m) -> processor cm_tl (UNIT :: s) m
(* error case for pop : empty list *)
| (POP :: cm_tl, sv :: s_tl, m) -> processor cm_tl s_tl m
| (POP :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
(* the code for pushing elements back wouldn't be necessary since we not  *)
(* error cases for add : invalid type, one element, empty list*)
| (ADD :: cm_tl, INT(x) :: INT(y) :: s_tl, m) -> processor cm_tl (INT (y+x) :: s_tl) m
| (ADD :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
(* error cases for sub : invalid type, one element, empty list *)
| (SUB :: cm_tl, INT(x) :: INT(y) :: s_tl, m) -> processor cm_tl (INT (y-x) :: s_tl) m
| (SUB :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
(* error cases for sub : empty list, one element, invalid type *)
| (MUL :: cm_tl, INT(x) :: INT(y) :: s_tl, m) -> processor cm_tl (INT (y*x) :: s_tl) m
| (MUL :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
(* error cases for sub : division by 0, empty list, one element, invalid type*)
| (DIV :: cm_tl, INT(0) :: INT(x) :: s_tl, m) -> processor cm_tl (ERROR :: s) m
| (DIV :: cm_tl, INT(x) :: INT(y) :: s_tl, m) -> processor cm_tl (INT(y/x) :: s_tl) m
| (DIV :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
(* error cases for rem : mod by 0, empty list, one element, invalid type*)
| (REM :: cm_tl, INT(x) :: INT(0) :: s_tl, m) -> processor cm_tl (ERROR :: s) m
| (REM :: cm_tl, INT(x) :: INT(y) :: s_tl, m) -> processor cm_tl (INT(y mod x) :: s_tl) m
| (REM :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (NEG :: cm_tl, INT(x) :: s_tl, m) -> processor cm_tl (INT(-x) :: s_tl) m
| (NEG :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (SWAP :: cm_tl, x :: y :: s_tl, m) -> processor cm_tl (y :: x :: s_tl) m
| (SWAP :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (ToString :: cm_tl, INT(x) :: s_tl, m) -> processor cm_tl (STRING(string_of_int(x)) :: s_tl) m
| (ToString :: cm_tl, BOOL(true) :: s_tl, m) -> processor cm_tl (STRING(":true:") :: s_tl) m
| (ToString :: cm_tl, BOOL(false) :: s_tl, m) -> processor cm_tl (STRING(":false:") :: s_tl) m
| (ToString :: cm_tl, ERROR :: s_tl, m) -> processor cm_tl (STRING(":error:") :: s_tl) m
| (ToString :: cm_tl, UNIT :: s_tl, m) -> processor cm_tl (STRING(":unit:") :: s_tl) m
| (ToString :: cm_tl, STRING(x) :: s_tl, m) -> processor cm_tl (quotation_filter x :: s_tl) m
| (ToString :: cm_tl, NAME(x) :: s_tl, m) -> processor cm_tl (STRING(x) :: s_tl) m
| (Println :: cm_tl, STRING(x) :: s_tl, m) -> write_to_file x processor cm_tl s_tl m


| (CAT :: cm_tl, STRING(x) :: STRING(y) :: s_tl, m) -> processor cm_tl (STRING(y^x) :: s_tl) m
| (CAT :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (AND :: cm_tl, BOOL(x) :: BOOL(y) :: s_tl, m) -> processor cm_tl (BOOL(y && x) :: s_tl) m
| (AND :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (OR :: cm_tl, BOOL(x) :: BOOL(y) :: s_tl, m) -> processor cm_tl (BOOL(y || x) :: s_tl) m
| (OR :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (NOT :: cm_tl, BOOL(x) :: s_tl, m) -> processor cm_tl (BOOL(not x) :: s_tl) m
| (NOT :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (EQUAL :: cm_tl, INT(x) :: INT(y) :: s_tl, m) -> processor cm_tl (BOOL(y == x) :: s_tl) m
| (EQUAL :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m
| (LESSTHAN :: cm_tl, INT(x) :: INT(y) :: s_tl, m) -> processor cm_tl (BOOL(y < x) :: s_tl) m
| (LESSTHAN :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m

(* error when 1. bind an identifier to an unbound identifier 2. if stack is empty *)
(* how about an identifer which its key already exists and pusing another identifer that has the same key*)
| (BIND :: cm_tl, ERROR :: NAME(k) :: s_tl, m) -> processor cm_tl (ERROR :: s) m
(* bind k to v of v*)
| (BIND :: cm_tl, NAME(v) :: NAME(k) :: s_tl, m) -> processor cm_tl (UNIT :: s_tl) m
| (BIND :: cm_tl, v :: NAME(k) :: s_tl, m) -> processor cm_tl (UNIT :: s_tl) (((NAME(k),v) :: l) :: m)

(*
| (BIND :: cm_tl, STRING(v) :: NAME(k) :: s_tl) -> processor cm_tl (UNIT :: s_tl) m
| (BIND :: cm_tl, BOOL(v) :: NAME(k) :: s_tl) -> processor cm_tl (UNIT :: s_tl) m
| (BIND :: cm_tl, UNIT :: NAME(k) :: s_tl) -> processor cm_tl (UNIT :: s_tl) m
| (BIND :: cm_tl, NAME(v) :: NAME(k) :: s_tl) -> processor cm_tl (UNIT :: s_tl) m
| (BIND :: cm_tl, st_tl) -> processor cm_tl (ERROR :: st_tl) m
*)



| (ADD :: cm_tl, NAME(x) :: NAME(y) :: s_tl, m) -> processor cm_tl (INT(y+x) :: s_tl) m
| (ADD :: cm_tl, INT(x) :: NAME(y) :: s_tl, m) -> processor cm_tl (INT(y+x) :: s_tl) m
| (ADD :: cm_tl, NAME(x) :: INT(y) :: s_tl, m) -> processor cm_tl (INT(y+x) :: s_tl) m

(*
| (SUB :: cm_tl, NAME(x) :: NAME(y) :: s_tl) -> processor cm_tl (INT(y-x) :: s_tl) m

| (MUL :: cm_tl, NAME(x) :: NAME(y) :: s_tl) -> processor cm_tl (INT(y*x) :: s_tl) m

| (DIV :: cm_tl, NAME(x) :: NAME(y) :: s_tl) -> processor cm_tl (INT(y/x) :: s_tl) m

| (REM :: cm_tl, NAME(x) :: NAME(y) :: s_tl) -> processor cm_tl (INT(y mod x) :: s_tl) m

| (NEG :: cm_tl, NAME(x) :: s_tl) -> processor cm_tl (INT(-x) :: s_tl) m

| (SWAP :: cm_tl, NAME(x) :: NAME(y) :: s_tl) -> processor cm_tl (NAME(y) :: NAME(x) :: s_tl) m

| (ToString :: cm_tl, NAME(x) :: s_tl) -> processor cm_tl (STRING(x) :: s_tl) m

*)

| (IF :: cm_tl, x :: y :: BOOL(true) :: s_tl, m) -> processor cm_tl (x :: s_tl) m
| (IF :: cm_tl, x :: y :: BOOL(false) :: s_tl, m) -> processor cm_tl (y :: s_tl) m
| (IF :: cm_tl, s, m) -> processor cm_tl (ERROR :: s) m

(* Will create a new environment *)
| (LET :: cm_tl, s, m) -> processor cm_tl s 

(* will close the environment *)
| (END :: cm_tl, s, m) -> processor cm_tl s 
 
(*| ([], _) -> processor sv_cmd_li (ERROR :: stack) *)
(* | _ -> processor sv_cmd_li (ERROR :: stack) *)
| ([], _, _) -> ()
| _ -> ()

in

processor sv_cmd_list [];;

(*
interpreter ("Projectpart1TestInputs/input1.txt","Projectpart1TestOutputs/my_output1.txt");
interpreter ("Projectpart1TestInputs/input2.txt","Projectpart1TestOutputs/my_output2.txt");
interpreter ("Projectpart1TestInputs/input3.txt","Projectpart1TestOutputs/my_output3.txt");
interpreter ("Projectpart1TestInputs/input4.txt","Projectpart1TestOutputs/my_output4.txt");
interpreter ("Projectpart1TestInputs/input5.txt","Projectpart1TestOutputs/my_output5.txt");
interpreter ("Projectpart1TestInputs/input6.txt","Projectpart1TestOutputs/my_output6.txt");
interpreter ("Projectpart1TestInputs/input7.txt","Projectpart1TestOutputs/my_output7.txt");
interpreter ("Projectpart1TestInputs/input8.txt","Projectpart1TestOutputs/my_output8.txt");
interpreter ("Projectpart1TestInputs/input9.txt","Projectpart1TestOutputs/my_output9.txt");
interpreter ("Projectpart1TestInputs/input10.txt","Projectpart1TestOutputs/my_output10.txt");;
*)
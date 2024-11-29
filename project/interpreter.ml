
type stackValue = STRING of string | INT of int | BOOL of bool
| ERROR | NAME of string | UNIT | CLOSURE of (stackValue * (command list) * (stackValue*stackValue)list list)

and 

command = ADD | SUB | MUL | DIV | REM | NEG | PUSH of stackValue | POP
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
  let convertible_to_float sv = match float_of_string_opt sv with | Some _ -> true | None -> false in

  (* a function that checks if stack value is string *)
  let convertible_to_str sv = if String.get sv 0 = '"' && String.get sv (String.length sv - 1) = '"' then true else false in

  (* evaluate stack value and if it is int, convert to int *)
  if convertible_to_int sv then PUSH (INT(int_of_string(sv)))
  
  (* evaluate stack value and if it is str, convert to str *)
  else if convertible_to_str sv then PUSH (STRING(sv))

  else if convertible_to_float sv then PUSH (ERROR)

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
  | "bind" :: tl -> BIND :: convert_strCmd_to_svCmd tl
  | "let" :: tl -> LET :: convert_strCmd_to_svCmd tl
  | "end" :: tl -> END :: convert_strCmd_to_svCmd tl
  | "cat" :: tl -> CAT :: convert_strCmd_to_svCmd tl
  | "and" :: tl -> AND :: convert_strCmd_to_svCmd tl
  | "or" :: tl -> OR :: convert_strCmd_to_svCmd tl
  | "not" :: tl -> NOT :: convert_strCmd_to_svCmd tl
  | "equal" :: tl -> EQUAL :: convert_strCmd_to_svCmd tl
  | "lessThan" :: tl -> LESSTHAN :: convert_strCmd_to_svCmd tl
  | "if" :: tl -> IF :: convert_strCmd_to_svCmd tl
  | push_cm :: tl -> push_convert_helper push_cm :: convert_strCmd_to_svCmd tl

in

(* let sv_cmd_list : command list = convert_strCmd_to_svCmd string_cmd_list;; *)

(* Convert string command list to implemented typed command list *)
let sv_cmd_list : command list = convert_strCmd_to_svCmd string_cmd_list

in

let write_to_file sv processor_func cm_tl ss mm: unit =
  file_write sv;
  processor_func cm_tl ss mm
in

let quotation_filter_for_tostring str = STRING (String.sub str 1 (String.length str - 2))

in

let quotation_filter_for_concat s1 s2 = STRING (String.sub s1 0 (String.length s1 - 1) ^ String.sub s2 1 (String.length s2 - 1))

in

let rec fetch (name: stackValue) (mm : (stackValue * stackValue) list list) : stackValue =
  match mm with
  (* waht happens when there is no matching key? *)
  (* keep search *)
  | [] :: mm_tl -> fetch name mm_tl
  | ((k, v) :: m) :: mm_tl -> if k = name then v else fetch name (m :: mm_tl)
  | _ -> ERROR

in

let rec processor (cmd_li: command list) (ss : stackValue list list) (mm : (stackValue * stackValue) list list) : unit =
match (cmd_li, ss , mm) with
(* whether int is positive or negative, done through this line *)
| (PUSH INT(sv) :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((INT(sv) :: s) :: ss_tl) mm
  (* from the input file string is enclosed with "". Then, output doesn't contain "" *)
| (PUSH STRING(sv) :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((STRING(sv) :: s) :: ss_tl) mm
  (* -------------- any error case? *)
| (PUSH NAME(sv) :: cm_tl, s :: ss_tl, _)-> processor cm_tl ((NAME(sv) :: s ) :: ss_tl) mm
| (PUSH BOOL(sv) :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((BOOL(sv) :: s) :: ss_tl) mm
| (PUSH ERROR :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm
| (PUSH UNIT :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((UNIT :: s) :: ss_tl) mm
(* error case for pop : empty list *)
| (POP :: cm_tl, (sv :: s) :: ss_tl, _) -> processor cm_tl (s :: ss_tl) mm
| (POP :: cm_tl, [] :: ss_tl, _) -> processor cm_tl ((ERROR :: []) :: ss_tl) mm
(* the code for pushing elements back wouldn't be necessary since we not  *)
(* error cases for add : invalid type, one element, empty list*)
| (ADD :: cm_tl, (INT(x) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((INT (y+x) :: s) :: ss_tl) mm
| (ADD :: cm_tl, (INT(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | INT(b) -> processor cm_tl ((INT(b+x) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: INT(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (ADD :: cm_tl, (NAME(x) :: INT(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(a) -> processor cm_tl ((INT(y+a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
)
| (ADD :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch ((NAME(x))) mm, fetch (NAME(y)) mm)  with
  | (INT(a), INT(b)) -> processor cm_tl ((INT(b+a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (ADD :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm
(* error cases for sub : invalid type, one element, empty list *)
| (SUB :: cm_tl, (INT(x) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((INT (y-x) :: s) :: ss_tl) mm
| (SUB :: cm_tl, (INT(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | INT(b) -> processor cm_tl ((INT(b-x) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: INT(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (SUB :: cm_tl, (NAME(x) :: INT(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(a) -> processor cm_tl ((INT(y-a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
)

| (SUB :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch ((NAME(x))) mm, fetch (NAME(y)) mm)  with
  | (INT(a), INT(b)) -> processor cm_tl ((INT(b-a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (SUB :: cm_tl, s :: ss_tl , _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm
(* error cases for sub : empty list, one element, invalid type *)
| (MUL :: cm_tl, (INT(x) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((INT (y*x) :: s) :: ss_tl) mm
| (MUL :: cm_tl, (INT(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | INT(b) -> processor cm_tl ((INT(x*b) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: INT(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (MUL :: cm_tl, (NAME(x) :: INT(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(a) -> processor cm_tl ((INT(a*y) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
)
| (MUL :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch ((NAME(x))) mm, fetch (NAME(y)) mm)  with
  | (INT(a), INT(b)) -> processor cm_tl ((INT(a*b) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (MUL :: cm_tl, s :: ss_tl , _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm
(* error cases for sub : division by 0, empty list, one element, invalid type*)
| (DIV :: cm_tl, (INT(0) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((ERROR :: INT(0) :: INT(y) :: s) :: ss_tl) mm
| (DIV :: cm_tl, (INT(0) :: NAME(y) :: s) :: ss_tl, _) -> processor cm_tl ((ERROR :: INT(0) :: NAME(y) :: s) :: ss_tl) mm
| (DIV :: cm_tl, (INT(x) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((INT(y/x) :: s) :: ss_tl) mm
| (DIV :: cm_tl, (INT(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | INT(b) -> processor cm_tl ((INT(b/x) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: INT(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (DIV :: cm_tl, (NAME(x) :: INT(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(0) -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
  | INT(a) -> processor cm_tl ((INT(y/a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
)
| (DIV :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch ((NAME(x))) mm, fetch (NAME(y)) mm)  with
  | (INT(0), INT(b)) -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
  | (INT(a), INT(b)) -> processor cm_tl ((INT(b/a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (DIV :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm
(* error cases for rem : mod by 0, empty list, one element, invalid type*)
| (REM :: cm_tl, (INT(0) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((ERROR :: INT(0) :: INT(y) :: s) :: ss_tl)  mm
| (REM :: cm_tl, (INT(0) :: NAME(y) :: s) :: ss_tl, _) -> processor cm_tl ((ERROR :: INT(0) :: NAME(y) :: s) :: ss_tl) mm
| (REM :: cm_tl, (INT(x) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((INT(y mod x) :: s) :: ss_tl) mm
| (REM :: cm_tl, (INT(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | INT(b) -> processor cm_tl ((INT(b mod x) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: INT(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (REM :: cm_tl, (NAME(x) :: INT(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(0) -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
  | INT(a) -> processor cm_tl ((INT(y mod a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
)
| (REM :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch ((NAME(x))) mm, fetch (NAME(y)) mm)  with
  | (INT(0), INT(b)) -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
  | (INT(a), INT(b)) -> processor cm_tl ((INT(b mod a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
)
| (REM :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

(* Negation works on variable? *)
| (NEG :: cm_tl, (INT(x) :: s) :: ss_tl, _) -> processor cm_tl ((INT(-x) :: s) :: ss_tl) mm
| (NEG :: cm_tl, (NAME(x) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(a) -> processor cm_tl ((INT(-a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: s) :: ss_tl) mm
)
| (NEG :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

| (SWAP :: cm_tl, (x :: y :: s) :: ss_tl, _) -> processor cm_tl ((y :: x :: s) :: ss_tl) mm
| (SWAP :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm
| (ToString :: cm_tl, (INT(x) :: s) :: ss_tl, _) -> processor cm_tl ((STRING(string_of_int(x)) :: s) :: ss_tl) mm
| (ToString :: cm_tl, (BOOL(true) :: s) :: ss_tl, _) -> processor cm_tl ((STRING(":true:") :: s) :: ss_tl) mm
| (ToString :: cm_tl, (BOOL(false) :: s) :: ss_tl, _) -> processor cm_tl ((STRING(":false:") :: s) :: ss_tl) mm
| (ToString :: cm_tl, (ERROR :: s) :: ss_tl, _) -> processor cm_tl ((STRING(":error:") :: s) :: ss_tl) mm
| (ToString :: cm_tl, (UNIT :: s) :: ss_tl, _) -> processor cm_tl ((STRING(":unit:") :: s) :: ss_tl) mm
| (ToString :: cm_tl, (STRING(x) :: s) :: ss_tl, _) -> processor cm_tl ((quotation_filter_for_tostring x :: s) :: ss_tl) mm
| (ToString :: cm_tl, (NAME(x) :: s) :: ss_tl, _) -> processor cm_tl ((STRING(x) :: s) :: ss_tl) mm
| (ToString :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm
| (Println :: cm_tl, (STRING(x) :: s) :: ss_tl, _) -> write_to_file x processor cm_tl (s::ss_tl) mm
| (Println :: cm_tl, s :: ss_tl, _) -> processor cm_tl ss mm

(* check name,name and error,name cases first to filter these from sv,name case *)
| (BIND :: cm_tl, (sv :: NAME(k) :: s) :: ss_tl, m :: mm_tl) -> (
  match sv with
  | NAME(v) -> (
              match fetch (NAME(v)) mm with 
              | ERROR -> processor cm_tl ((ERROR :: sv :: NAME(k) :: s) :: ss_tl) mm
              | fetched_value -> processor cm_tl ((UNIT :: s) :: ss_tl) (((NAME(k), fetched_value) :: m) :: mm_tl)
              )
  | ERROR -> processor cm_tl ((ERROR :: ERROR :: NAME(k) :: s) :: ss_tl) mm
  | value -> processor cm_tl ((UNIT :: s) :: ss_tl) (((NAME(k), value) :: m) :: mm_tl)
  )
  | (BIND :: cm_tl, s :: ss_tl, m :: mm_tl) ->
    processor cm_tl ((ERROR :: s) :: ss_tl) mm
  
| (LET :: cm_tl, s :: ss_tl, _) -> processor cm_tl (s :: ss) ([] :: mm)
(*
| (LET :: cm_tl, s :: ss_tl, _) -> processor cm_tl (s :: ss) ([] :: mm)*)
| (END :: cm_tl, (sv :: s) :: prev_s :: ss_tl, m :: mm_tl) -> processor cm_tl ((sv :: prev_s) :: ss_tl) mm_tl
(* when the most top value is variable, return the variable or value?*)

| (CAT :: cm_tl, (STRING(x) :: STRING(y) :: s) :: ss_tl, _) -> processor cm_tl ((quotation_filter_for_concat y x :: s):: ss_tl) mm
| (CAT :: cm_tl, (STRING(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | STRING(b) -> processor cm_tl ((quotation_filter_for_concat b x :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: STRING(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (CAT :: cm_tl, (NAME(x) :: STRING(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | STRING(a) -> processor cm_tl ((quotation_filter_for_concat y a :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: STRING(y) :: s) :: ss_tl) mm
) 
| (CAT :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch (NAME(x)) mm, fetch (NAME(y)) mm) with
  | (STRING(a), STRING(b)) -> processor cm_tl ((quotation_filter_for_concat b a :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (CAT :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

| (AND :: cm_tl, (BOOL(x) :: BOOL(y) :: s) :: ss_tl, _) -> processor cm_tl ((BOOL(x && y) :: s) :: ss_tl) mm
| (AND :: cm_tl, (BOOL(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | BOOL(b) -> processor cm_tl ((BOOL(x && b) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: BOOL(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (AND :: cm_tl, (NAME(x) :: BOOL(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | BOOL(a) -> processor cm_tl ((BOOL(a && y) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: BOOL(y) :: s) :: ss_tl) mm
) 
| (AND :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch (NAME(x)) mm, fetch (NAME(y)) mm) with
  | (BOOL(a), BOOL(b)) -> processor cm_tl ((BOOL(a && b) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (AND :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

| (OR :: cm_tl, (BOOL(x) :: BOOL(y) ::s) :: ss_tl, _) -> processor cm_tl ((BOOL(x || y) :: s) :: ss_tl) mm
| (OR :: cm_tl, (BOOL(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | BOOL(b) ->  processor cm_tl ((BOOL(x || b) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: BOOL(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (OR :: cm_tl, (NAME(x) :: BOOL(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | BOOL(a) ->   processor cm_tl ((BOOL(a || y) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: BOOL(y) :: s) :: ss_tl) mm
) 
| (OR :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch (NAME(x)) mm, fetch (NAME(y)) mm) with
  | (BOOL(a), BOOL(b)) ->   processor cm_tl ((BOOL(a || b) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (OR :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

| (NOT :: cm_tl, (BOOL(x) :: s) :: ss_tl, _) -> processor cm_tl ((BOOL(not x) :: s) :: ss_tl) mm
| (NOT :: cm_tl, (NAME(x) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | BOOL(a) -> processor cm_tl ((BOOL(not a) :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: s) :: ss_tl) mm
)
| (NOT :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

| (EQUAL :: cm_tl, (INT(x) :: INT(y) :: s) :: ss_tl, _) -> processor cm_tl ((BOOL(x = y) :: s) :: ss_tl) mm
| (EQUAL :: cm_tl, (INT(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | INT(b) -> processor cm_tl ((BOOL(x = b) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: INT(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (EQUAL :: cm_tl, (NAME(x) :: INT(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(a) -> processor cm_tl ((BOOL(a = y) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
) 
| (EQUAL :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch (NAME(x)) mm, fetch (NAME(y)) mm) with
  | (INT(a), INT(b)) ->   processor cm_tl ((BOOL(a = b) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (EQUAL :: cm_tl, s :: ss_tl, _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

| (LESSTHAN :: cm_tl, (INT(x) :: INT(y) :: s):: ss_tl, _) -> processor cm_tl ((BOOL(y < x) :: s) :: ss_tl) mm
| (LESSTHAN :: cm_tl, (INT(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(y)) mm with
  | INT(b) ->  processor cm_tl ((BOOL(b < x) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: INT(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (LESSTHAN :: cm_tl, (NAME(x) :: INT(y) :: s) :: ss_tl, _) -> (
  match fetch (NAME(x)) mm with
  | INT(a) ->   processor cm_tl ((BOOL(y < a) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: INT(y) :: s) :: ss_tl) mm
) 
| (LESSTHAN :: cm_tl, (NAME(x) :: NAME(y) :: s) :: ss_tl, _) -> (
  match (fetch (NAME(x)) mm, fetch (NAME(y)) mm) with
  | (INT(a), INT(b)) ->   processor cm_tl ((BOOL(b < a) :: s):: ss_tl) mm
  | _ -> processor cm_tl ((ERROR :: NAME(x) :: NAME(y) :: s) :: ss_tl) mm
) 
| (LESSTHAN :: cm_tl, s :: ss_tl , _) -> processor cm_tl ((ERROR :: s) :: ss_tl) mm

| (IF :: cm_tl, (x :: y :: BOOL(true) :: s) :: ss_tl, _) -> processor cm_tl ((x :: s) :: ss_tl) mm
| (IF :: cm_tl, (x :: y :: BOOL(false) :: s) :: ss_tl, _) -> processor cm_tl ((y :: s) :: ss_tl) mm
| (IF :: cm_tl, (x :: y :: NAME(z) :: s) :: ss_tl, _) -> (
  match fetch (NAME(z)) mm with 
  | BOOL(true) -> processor cm_tl ((x :: s) :: ss_tl) mm
  | BOOL(false) -> processor cm_tl ((y :: s) :: ss_tl) mm
  | _ -> processor cm_tl ((x :: y :: NAME(z) :: s) :: ss_tl) mm
)
| (IF :: cm_tl, s :: ss_tl , _) -> processor cm_tl ((ERROR :: s) :: ss) mm

| ([], _, _) -> ()
| _ -> ()

in

processor sv_cmd_list [[]] [[]];;

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


(* interpreter ("part2input/input1.txt", "part2output/my_output1.txt");
interpreter ("part2input/input2.txt", "part2output/my_output2.txt");
interpreter ("part2input/input3.txt", "part2output/my_output3.txt");
interpreter ("part2input/input4.txt", "part2output/my_output4.txt");
interpreter ("part2input/input5.txt", "part2output/my_output5.txt");
interpreter ("part2input/input6.txt", "part2output/my_output6.txt");
interpreter ("part2input/input7.txt", "part2output/my_output7.txt");
interpreter ("part2input/input8.txt", "part2output/my_output8.txt");
interpreter ("part2input/input9.txt", "part2output/my_output9.txt");
interpreter ("part2input/input10.txt", "part2output/my_output10.txt");
interpreter ("part2input/input11.txt", "part2output/my_output11.txt");;
interpreter ("part2input/input12.txt", "part2output/my_output12.txt");
interpreter ("part2input/input13.txt", "part2output/my_output13.txt");
interpreter ("part2input/input14.txt", "part2output/my_output14.txt");
interpreter ("part2input/input15.txt", "part2output/my_output15.txt");; *)
(* interpreter ("part2input/custom_input1.txt", "part2output/custom_output1.txt");; *)
(* interpreter ("part2input/custom_input2.txt", "part2output/custom_output2.txt");; *)
(* interpreter ("part2input/input11.txt", "part2output/my_output11.txt");; *)
(* interpreter ("part2input/custom_input1.txt", "part2output/custom_output1.txt");; *)


(* Week 2 Pattern matchhing *)

(* y is being defined by parttern matching *)
(* the result is used to print out the value of y *)
let y = Some (Some 1) in

  let result =
  match y with
  | None -> false
  | Some x ->
            match x with
              Some z -> true
            | None -> false
  in

  Printf.printf "%b\n" result;;



(* Week3 Inductive data types and Polymorphism *)

(* We can use data types to define inductive data *)
(* A binary tree is
  - a Leaf containing no data
  - a Node containing a key, a value a left subtree and a right subtree
    
An inductive data type T is a data type defined by:
1- a collection of cases that do not refer to T. In this case, this refers to Leaf
2- a collection of cases that build values of type T from other data of type T. In this case, this refers to Node
    *)
type key = string
type value = int
type tree = Leaf | Node of key * value * tree * tree




let e1 : int list = 1 :: [];;
let e2 : int list = 1 :: [];;
e1 @ e2;;
let e1 : int list = 1 :: [];;
let e2 : int list list = [1] :: [];;

(* It is possible to append multiple items with :: *)
let x : (int * bool) list = (1,true) :: (2, false) :: [];;
       
(* Although the types are all specified, it is possible to pass the value with empty arguments inside *)
let y : (int * (bool list)) list = [(5, [true])];;
let y : (int * (bool list)) list = [];;



(* Recursive Function *)
let rec foo (a:int) (b:int) (xs: bool list)  : int list =
match xs with 
(* base case *)
| [] -> []
(* inductive case *)
| hd::tl -> (if hd then a else b) :: (foo a b tl);;
(* This function above is equivalent to *)
(* List.map maps each element of xs to the anonymous function that takes each element as an arugment x *)
let foo (a:int) (b:int) (xs: bool list) = List.map (fun x -> if x then a else b) xs



(* The empty list case can be checked with if statement like below, but the pattern exhaustive warning still shows *)
let rec foo (xs: int list)  : int list =
  if xs = [] then [] else
    match xs with 
    | hd::tl -> hd + 1 :: (foo tl)
(* The above code is equivalent to this 
List.map method takes into account of the case that the argument list is empty *)
let foo (xs: int list) = List.map (fun x -> x + 1) xs

let foo  = fun a -> fun b -> fun xs -> (a+b)::xs;;



(* Polymorphism *)
(* Polymorphism is the concept of abstracting the types of argument of a function, so that the function can be resued again with different types 
We say map is polymorphic in the types 'a and 'b *)
let rec map (f: 'a -> 'b) (xs: 'a list) : 'b list =
  match xs with
  | [] -> []
  | hd :: tl -> (f hd)::(map f tl);;

(* - : ('a -> 'b -> 'b ) -> 'a list -> 'b -> 'b = <fun> *)
List.fold_right

let add x y = x + y
 let rec reduce (f: int->int->int) (b:int) (xs: int list) : int =
  match xs with
  | [] -> b
  | hd::tl -> f hd (reduce f b tl);;

let sum xs = reduce add 0 xs
let sum xs = reduce (fun x y -> x+y) 0 xs



(* More on Anonymous Functions *)
(* We figure out these are all the same functions as the type val -> val -> val indicates *)
(* Ocaml, in fact, takes only one argument at a time *)
(* Writing code that intends to take only one argument is called currying *)
let add x y = x+y (* this function is syntactic sugar for*)
let add = (fun x y -> x+y)
let add = (fun x -> (fun y -> x+y))
let add = (fun x -> fun y -> x+y)



(* Week 4 Higher order FUnctions and Curring *)

(* The advantage of currying
1. Partical Appliation
2. More easily compose functions *)

(* Curried functions allow defs of new, partially applied functions *)
let inc = add 1
(* which is equivalent to *)
let inc = (fun y -> 1 + y)
(* which is equivalent to *)
let inc y = 1 + y


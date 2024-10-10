(* Week 1 *)

(* Ocaml is an expression based language, not a value based lanauage. *)

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

(* Inductive data type refers to a data structure that is defiend recursively *)

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
(* This is an inductive data type *)
type tree = Leaf | Node of key * value * tree * tree

(* You can start tree with an empty Leaf 
In the case of else at the end, the function will simply replace the Node with given value 
*)
let rec insert (t: tree) (k: key) (v:value) : tree = 
  match t with
  | Leaf -> Node (k, v, Leaf, Leaf) 
  | Node (nk, nv, left, right) ->
      if k < nk then
        Node (nk, nv, insert left k v, right)
      else if k > nk then
        Node (nk, nv, left, insert right k v)
      else
        Node (k, v, left, right)

      
(* List in Ocaml *)
let e1 : int list = 1 :: [];;
let e2 : int list = 1 :: [];;
e1 @ e2;;
let e1 : int list = 1 :: [];;
let e2 : int list list = [1] :: [];;

(* It is possible to append multiple items with :: *)
let x : (int * bool) list = (1,true) :: (2, false) :: [];;
       
(* Although the types are all specified, it is possible to leave the argument empty inside a list *)
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
List.map method considers the case that the argument list is empty *)
let foo (xs: int list) = List.map (fun x -> x + 1) xs


let foo (a:int) (b:int) (xs: int list) : int list = (a + b) :: xs;;
(* is equivalent to *)
let foo  = fun a -> fun b -> fun xs -> (a+b)::xs;;
(* which can be also written as *)
let foo  = fun a b -> fun xs -> (a+b)::xs;;


(* Polymorphism *)

(* Polymorphism is the concept of abstracting the types of argument of a function, so that the function can be resued again with different types 
We say map is polymorphic in the types 'a and 'b *)
let rec map (f: 'a -> 'b) (xs: 'a list) : 'b list =
  match xs with
  | [] -> []
  | hd :: tl -> (f hd)::(map f tl);;


(* Here for the f argument, we define a function, such as a + b *)
(*('a -> 'b -> 'b ) is the type of a defined function*)
let rec fold_left f acc xs =
  match xs with
  | [] -> acc
  | hd :: tl -> fold_left f (f acc hd) tl

  
let rec fold_right f xs acc =
  match xs with
  | [] -> acc
  | hd::tl -> f hd (fold_right f tl acc)


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

(* Types and functions *)
let add x y = x + y;;
(add 2) 3 (* returning 5 since (add 2) returns a function that takes y and returns x + y *)



(* The advantage of currying
1. Partical Appliation
2. More easily compose functions *)

(* Curried functions allow defs of new, partially applied functions *)
(* add 1 is returning a function that has the type value -> value*)
let inc = add 1
(* which is equivalent to *)
let inc = (fun y -> 1 + y)
(* which is equivalent to *)
let inc y = 1 + y


(* CSE322 Compiler Assignment 0 - Task 2 *)

exception NotImplemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

(* sum t: return the sum of all values *)
let rec sum t = match t with
  | Leaf x -> x
  | Node (t1,x,t2) -> (sum t1) + x + (sum t2)

(* exist t n: return true if n exists in a tree *)
let rec exist t n = raise NotImplemented

(* count t n: count n in a tree *)
let rec count t n = raise NotImplemented

(* inorder t: return the list of values using inorder tree traversal *)
let rec inorder t = raise NotImplemented

(* depth t: return the depth of a tree*)
let rec depth t = raise NotImplemented

(* max t: return the maximum value in a tree*)
let rec max t = raise NotImplemented

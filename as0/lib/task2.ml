(* CSE322 Compiler Assignment 0 - Task 2 *)

exception NotImplemented

(* Binary Tree: 
  A polymorphic binary tree data structure where each node can be either:
  - Leaf: contains a value of type 'a with no children
  - Node: contains a value of type 'a and two subtrees *)
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

(* sum t: calculates the sum of all values in the tree
  - For a Leaf, returns its value
  - For a Node, recursively sums left subtree + current value + right subtree *)
let rec sum t = match t with
  | Leaf x -> x
  | Node (t1,x,t2) -> (sum t1) + x + (sum t2)

(* exist t n: checks if value n exists anywhere in the tree
  - For a Leaf, checks if its value equals n
  - For a Node, checks current value or recursively searches left and right subtrees *)
let rec exist t n = match t with
  | Leaf x -> x = n
  | Node (t1, x, t2) -> x = n || exist t1 n || exist t2 n

(* count t n: counts occurrences of value n in the tree
  - For a Leaf, returns 1 if its value equals n, otherwise 0
  - For a Node, counts occurrences in current node + left subtree + right subtree *)
let rec count t n = match t with
  | Leaf x -> if x = n then 1 else 0
  | Node (t1, x, t2) ->
    (if x = n then 1 else 0) + count t1 n + count t2 n

(* inorder t: performs inorder traversal to create a list of values
  - For a Leaf, returns a singleton list with its value
  - For a Node, concatenates [left subtree values] + [current value] + [right subtree values] *)
let rec inorder t = match t with
  | Leaf x -> [x]
  | Node (t1, x, t2) -> (inorder t1) @ [x] @ (inorder t2)

(* depth t: calculates the maximum depth (height) of the tree
  - For a Leaf, depth is 0
  - For a Node, depth is 1 + max(depth of left subtree, depth of right subtree) *)
let rec depth t = match t with
  | Leaf _ -> 0
  | Node (t1, _, t2) ->
    let d1 = depth t1 in
    let d2 = depth t2 in
    1 + (if d1 > d2 then d1 else d2)

(* max t: finds the maximum value in the tree
  - For a Leaf, returns its value
  - For a Node, compares current value with max values from left and right subtrees *)
let rec max t = match t with
  | Leaf x -> x
  | Node (t1, x, t2) ->
    let max_left = max t1 in
    let max_right = max t2 in
    let max_child = if max_left > max_right then max_left else max_right in
    if x > max_child then x else max_child
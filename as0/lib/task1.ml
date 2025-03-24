(* CSE322 Compiler Assignment 0 - Task 1 *)

exception NotImplemented

(* 1. Basic Recursion *)

(* sum n: calculate 1 + 2 + ... + n *)
let rec sum n = match n with
  | 0 -> 0
  | n -> n + sum (n-1)

(* fac n: calculate 1 * 2 * ... * n *)
let rec fac n = raise NotImplemented

(* fib n: return the n-th fibonacci number *)
let rec fib n = raise NotImplemented

(* pow (x, y): calculate x to the power of y *)
let rec pow (x, y) = raise NotImplemented

(* gcd (x, y): find the great common divisor of x and y *)
let rec gcd (x, y) = raise NotImplemented

(* palindrome s: return true if s is a palindrome *)
let rec palindrome s = raise NotImplemented

(* 2. List *)

(* max l: return the maximum value in l *)
let rec max l = match l with
  | [] -> 0
  | [x] -> x
  | hd::tl -> if hd > max tl then hd else max tl

(* exist l x: check if x exists in l *)
let rec exist l x = raise NotImplemented

(* count l x: count the number of x in l *)
let rec count l x = raise NotImplemented

(* reverse l: return the reversed l *)
let rec reverse l = raise NotImplemented

(* find l x: return the index of the first x in l
*                   -1 if x does not exist in l *)
let rec find l x = raise NotImplemented

(* findr l x: return the index of the last x in l
*                    -1 if x does not exist in l *)
let rec findr l x = raise NotImplemented



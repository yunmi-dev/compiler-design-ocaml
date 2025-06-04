(* CSE322 Compiler Assignment 3 *)
(* Type Checker *)

module A = Absyn

let rec list2string l = match l with
  | [] -> ""
  | [t] -> t
  | h::t -> h ^ "," ^ list2string t

let rec tp2string tp = match tp with
  | A.Inttp -> "int"
  | A.Tupletp tps -> "<" ^ (list2string (List.map tp2string tps)) ^ ">"
  | A.Arrowtp (tp1, tp2) -> tp2string tp1 ^ " -> " ^ tp2string tp2
  | A.Reftp tp -> tp2string tp ^ " ref"

type context = A.tp Symbol.table

exception Unimplemented

(* sub: check if t1 is a subtype of t2 *)
let rec sub (t1,t2) =
  let width(tl1, tl2) = if (List.length tl1) >= (List.length tl2) then true else false in
  let rec depth(tl1, tl2) = match tl2 with
    | tp2::tl2' -> (match tl1 with tp1::tl1'-> if sub(tp1,tp2) then depth(tl1',tl2') else false | _ -> false)
    | [] -> true
  in match t1 with
    | A.Inttp -> if t2 = A.Inttp then true else false
    | A.Tupletp tps1 -> (match t2 with
      | A.Tupletp tps2 -> if width(tps1, tps2) then depth (tps1, tps2) else false
      | _ -> false)
    | A.Arrowtp (t1p1, t1p2) -> (match t2 with
      | A.Arrowtp (t2p1, t2p2) -> if sub (t2p1,t1p1) && sub(t1p2,t2p2) then true else false
      | _ -> false)
    | A.Reftp tp1 -> (match t2 with
      | A.Reftp tp2 -> if tp1 = tp2 then true else false
      | _ -> false)

(* check_sub: raise error if t1 is not a subtype of t2 *)
let check_sub pos (tp1, tp2) =
   if sub (tp1, tp2) then ()
   else Errormsg.error (pos, (tp2string tp1)^" is not a subtype of "^(tp2string tp2))

(* complain: alias for Errormsg.error *)
let complain pos err = Errormsg.error (pos, err)

(* join: compute the join of t1 and t2 *)
let rec join pos (t1,t2) : A.tp =
  (* check if st1 and st2 can be joined or not *)
  let rec check_join (st1, st2) = match st1 with
    | A.Inttp -> if sub(st1,st2) then true else false
    | A.Tupletp tps1 -> (match st2 with
      | A.Tupletp tps2 -> true
      | _ -> false)
    | A.Arrowtp (t1p1, t1p2) -> (match st2 with
      | A.Arrowtp (t2p1, t2p2) -> check_join(t1p1,t2p1) && check_join(t1p2,t2p2)
      | _ -> false)
    | A.Reftp tp1 -> (match st2 with
      | A.Reftp tp2 -> if tp1=tp2 then true else false
      | _ -> false) in
  (* join tl1 list and tl2 list *)
  let rec rec_join (tl1, tl2) =
    match (tl1, tl2) with
    | (tp1::tl1', tp2::tl2') -> if check_join(tp1,tp2) then join pos (tp1,tp2)::(rec_join (tl1', tl2'))
                                else []
    | ([], _) -> []
    | (_, []) -> []
  in match t1 with
    | A.Inttp -> if sub(t1,t2) then A.Inttp
                 else (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1)
    | A.Tupletp tps1 -> (match t2 with
      | A.Tupletp tps2 -> A.Tupletp (rec_join (tps1, tps2))
      | _ -> (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))
    | A.Arrowtp (t1p1, t1p2) -> (match t2 with
      | A.Arrowtp (t2p1, t2p2) -> A.Arrowtp (join pos (t1p1,t2p1), join pos (t1p2,t2p2))
      | _ -> (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))
    | A.Reftp tp1 -> (match t2 with
      | A.Reftp tp2 -> if tp1 = tp2 then t1
                       else (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1)
      | _ -> (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))

(* tc_exp: check the type of the given expression e *)
(* - ctxt: symbol table                             *)
(* - pos: position of the expression                *)
let rec tc_exp ctxt pos e : A.tp = 
  match e with
  (* 정수 리터럴 *)
  | A.Int _ -> A.Inttp
  
  (* 식별자 *)
  | A.Id id -> (
      match Symbol.find id ctxt with
      | Some tp -> tp
      | None -> 
          complain pos ("undefined variable " ^ (Symbol.name id));
          A.Inttp
    )
  
  (* 연산자 *)
  | A.Op (oper, exps) -> (
      match oper, exps with
      (* 단항 마이너스 (0 - e2 형태로 파싱됨) *)
      | A.Sub, [A.Int 0; e2] ->
          let tp2 = tc_exp ctxt pos e2 in
          check_sub pos (tp2, A.Inttp);
          A.Inttp
      (* ref 생성자 *)
      | A.Ref, [e1] ->
          let tp1 = tc_exp ctxt pos e1 in
          A.Reftp tp1
      (* 역참조 *)
      | A.Get, [e1] ->
          let tp1 = tc_exp ctxt pos e1 in
          (match tp1 with
           | A.Reftp tp -> tp
           | _ -> 
               complain pos ("expected ref type, got " ^ (tp2string tp1));
               A.Inttp)
      
      (* 이항 산술 연산자 *)
      | A.Add, [e1; e2] | A.Sub, [e1; e2] | A.Mul, [e1; e2] ->
          let tp1 = tc_exp ctxt pos e1 in
          let tp2 = tc_exp ctxt pos e2 in
          check_sub pos (tp1, A.Inttp);
          check_sub pos (tp2, A.Inttp);
          A.Inttp
      (* 비교 연산자 *)
      | A.Eq, [e1; e2] | A.LT, [e1; e2] ->
          let tp1 = tc_exp ctxt pos e1 in
          let tp2 = tc_exp ctxt pos e2 in
          check_sub pos (tp1, A.Inttp);
          check_sub pos (tp2, A.Inttp);
          A.Inttp
      (* 대입 연산자 *)
      | A.Set, [e1; e2] ->
          let tp1 = tc_exp ctxt pos e1 in
          let tp2 = tc_exp ctxt pos e2 in
          (match tp1 with
           | A.Reftp tp ->
               check_sub pos (tp2, tp);
               A.Tupletp []
           | _ ->
               complain pos ("expected ref type in assignment, got " ^ (tp2string tp1));
               A.Tupletp [])
      | _ ->
          complain pos "invalid operator usage";
          A.Inttp
    )
  
  (* 튜플 *)
  | A.Tuple exps ->
      let tps = List.map (tc_exp ctxt pos) exps in
      A.Tupletp tps
  
  (* 프로젝션 *)
  | A.Proj (i, e1) ->
      let tp1 = tc_exp ctxt pos e1 in
      (match tp1 with
       | A.Tupletp tps ->
           if i >= 0 && i < List.length tps then
             List.nth tps i
           else (
             complain pos ("projection index " ^ (string_of_int i) ^ " out of bounds");
             A.Inttp)
       | _ ->
           complain pos ("expected tuple type for projection, got " ^ (tp2string tp1));
           A.Inttp)
  
  (* 조건문 *)
  | A.If (e1, e2, e3) ->
      let tp1 = tc_exp ctxt pos e1 in
      let tp2 = tc_exp ctxt pos e2 in
      let tp3 = tc_exp ctxt pos e3 in
      check_sub pos (tp1, A.Inttp);
      join pos (tp2, tp3)
  
  (* While 루프 *)
  | A.While (e1, e2) ->
      let tp1 = tc_exp ctxt pos e1 in
      let tp2 = tc_exp ctxt pos e2 in
      check_sub pos (tp1, A.Inttp);
      check_sub pos (tp2, A.Tupletp []);
      A.Tupletp []
  
  (* 함수 호출 *)
  | A.Call (e1, e2) ->
      let tp1 = tc_exp ctxt pos e1 in
      let tp2 = tc_exp ctxt pos e2 in
      (match tp1 with
       | A.Arrowtp (arg_tp, ret_tp) ->
           check_sub pos (tp2, arg_tp);
           ret_tp
       | _ ->
           complain pos ("expected function type for call, got " ^ (tp2string tp1));
           A.Inttp)
  
  (* Let 바인딩 *)
  | A.Let (id, e1, e2) ->
      let tp1 = tc_exp ctxt pos e1 in
      let ctxt' = Symbol.add id tp1 ctxt in
      tc_exp ctxt' pos e2
  
  (* 타입 제약 *)
  | A.Constrain (e1, tp) ->
      let tp1 = tc_exp ctxt pos e1 in
      check_sub pos (tp1, tp);
      tp
  
  (* 위치 정보가 있는 표현식 *)
  | A.Pos (new_pos, e1) ->
      tc_exp ctxt new_pos e1

(* tc_fundec: check the type of the function definition *)
let tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
  let ctxt' = Symbol.add x tp1 ctxt in
  let tp = tc_exp ctxt' pos exp
  in check_sub pos (tp, tp2)

(* do_another_fun: update the types of the functions *)
let do_another_fun ctxt (pos, fdec) =
  let (f, x, tp1, tp2, exp) = fdec in
  match Symbol.find f ctxt with
    (* check if the function name is duplicated *)
    | Some x -> (Errormsg.error(pos,"function name (" ^ Symbol.name f ^ ") is duplicated"); ctxt)
    | None -> if (Symbol.name f) = "main" then (* check if main function has int->int type *)
                (if (tp1 = A.Inttp) && (tp2 = A.Inttp) then Symbol.add f (A.Arrowtp (tp1, tp2)) ctxt
                 else (Errormsg.error(pos,"main function has wrong type"); Symbol.add f (A.Arrowtp (tp1, tp2)) ctxt))
              else Symbol.add f (A.Arrowtp (tp1, tp2)) ctxt

(* build_global_context: generate the initial symbol table *)
let build_global_context (fundecs): context =
  List.fold_left do_another_fun (Symbol.add (Symbol.symbol "print_int") (A.Arrowtp (A.Inttp, A.Tupletp [])) Symbol.empty) fundecs

(* tc: check the type of the program *)
let tc (fundecs : A.prog)  =
  let ctxt = build_global_context(fundecs) in
  let _ = List.map (tc_fundec ctxt) fundecs in
  ()
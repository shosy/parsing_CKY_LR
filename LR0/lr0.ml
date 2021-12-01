(* grammar *)
type t = string  (* terminal *)
type nt = string  (* non-terminal *)
type var = T of t | NT of nt  (* variable *)
type rule = nt * var list  (* non-terminal -> variables *)
type grammar = rule list * nt  (* rules and initial non-terminal *)

(* parse tree *)
type ptree = PTt of t | PTnt of nt * ptree list

type pos = int  (* 規則の何文字目を読んでいるか *)
type state = (rule * pos) list  (* LRオートマトンの状態 *)

type action = Shift of state | Reduce of rule | Accept


let prime = "'"
let eof = "$"


(** pretty-print function **)
let print_var = function 
    T(t) -> print_string t 
  | NT(nt) -> print_string nt

let print_rule (nt,vars) =
  print_string nt;
  print_string " -> ";
  let rec print_vars = function
      [] -> ()
    | [var] -> print_var var
    | var::vars -> print_var var; print_string " "; print_vars vars
  in
  print_vars vars

let rec print_state state =
  let print_rulepos (rule,pos) =
    let (nt,vars) = rule in
    print_string nt;
    print_string " -> ";
    let rec print_vars_pos n vars =
      (if n = 0 then 
        (print_string "."; if vars <> [] then print_string " "));
      match vars with
          [] -> ()
        | [var] -> print_var var
        | var::rest -> print_var var; print_string " "; print_vars_pos (n-1) rest
    in
    print_vars_pos pos vars
  in
  match state with
  | [] -> ()
  | [item] -> print_rulepos item
  | item::items -> print_rulepos item; print_string ",  "; print_state items

let print_ptree ptree =
  let rec print_tab n = 
    if n = 0 then () 
    else (print_string "\t"; print_tab (n-1))
  in
  let rec print_ptree_sub indent = function
      PTt(t) -> 
        print_tab indent;
        print_string t; 
        print_newline ()
    | PTnt(nt, ptrees) -> 
        print_tab indent;
        print_string nt;
        print_newline ();
        List.iter (fun ptree -> print_ptree_sub (indent+1) ptree) ptrees
  in
  print_ptree_sub 0 ptree


(**  **)
(* 文法規則allrulesのもとで、stateのクロージャを計算 *)
let closure state allrules = 
  let epsilon_transition (rule,pos) =
    let (nt,vars) = rule in
    if pos >= List.length vars then
      []
    else
      match List.nth vars pos with
        T(_) -> []
      | NT(nt) -> 
          let rules' = List.filter (fun (nt', _) -> nt' = nt) allrules in
          List.map (fun rule -> (rule,0)) rules'
  in
  let rec closure_aux state checked = 
    match state with
      [] -> List.sort compare checked
    | item::items -> 
        if List.mem item checked then
          closure_aux items checked
        else
          let new_items = epsilon_transition item in
          closure_aux (new_items@items) (item::checked)
  in
  closure_aux state []

(* 状態stateで変数varを読んだ後に遷移する状態 *)
let goto state var allrules =
  let state' = List.filter 
                (fun ((nt,vars),pos) -> 
                  (List.length vars > pos) && (List.nth vars pos = var)) 
                state 
  in
  let state'' = List.map (fun (rule,pos) -> (rule,pos+1)) state' in
  closure state'' allrules

(* 状態stateのときにreduceできる文法規則の集合 *)
let reduces state =
  let state' = List.filter 
                (fun ((_,vars),pos) -> List.length vars = pos)
                state
  in
  List.map fst state'

(*  *)
let lookup_LR0tbl_t state token allrules =
  let next_state = goto state (T(token)) allrules in
  let reducible_rules = reduces state in
  if next_state = [] then
    match reducible_rules with
    | [rule] -> Reduce(rule)
    | [] -> failwith "no action"
    | _ -> failwith "reduce/reduce conflict"
  else
    if reducible_rules = [] then
      if token = eof then Accept else Shift(next_state)
    else
      failwith "shift/reduce conflict"

(*  *)
let lookup_LR0tbl_nt state nt allrules =
  let next_state = goto state (NT(nt)) allrules in
  if next_state = [] then failwith "no action"
  else next_state

(** LR(0)の本体 **)
let rec lr state_stack ptree_stack tokens allrules =
  let curr_state = List.hd state_stack in
  print_string "state:\t"; print_state curr_state; print_newline ();
  let curr_token, rest_tokens = List.hd tokens, List.tl tokens in 
  print_string "read:\t"; print_string curr_token; print_newline ();
  let action = lookup_LR0tbl_t curr_state curr_token allrules in
  match action with
    Shift(next_state) -> 
      print_string "action:\tshift"; print_newline ();
      let next_state_stack = next_state :: state_stack in
      let next_ptree_stack = PTt(curr_token) :: ptree_stack in
      lr next_state_stack next_ptree_stack rest_tokens allrules
  | Reduce(rule) ->
      print_string "action:\treduce by "; print_rule rule; print_newline ();
      let (nt, vars) = rule in
      let pop stack n = 
        let rec pop_aux stack n ret = 
          if n = 0 then (ret, stack) 
          else pop_aux (List.tl stack) (n - 1) ((List.hd stack) :: ret) 
        in
        pop_aux stack n [] 
      in
      let n = List.length vars in
      let next_state_stack = 
        let (_, state_stack') = pop state_stack n in
        let next_state = lookup_LR0tbl_nt (List.hd state_stack') nt allrules in
        next_state :: state_stack' in
      let next_ptree_stack = 
        let (ptrees, ptree_stack') = pop ptree_stack n in
        (* 本当はptreesの根とvarsが同じか見たほうが良い *)
        PTnt(nt, ptrees) :: ptree_stack' in
      lr next_state_stack next_ptree_stack tokens allrules
  | Accept -> 
      print_string "action:\taccept"; print_newline ();
      match ptree_stack with
        [ptree] -> ptree
      | _ -> failwith "not only one parse tree"


(** initialization function **)
(* もとのinitial non-terminalを S としたとき、
   規則 S' -> S$ を加え、
   initial non-terminalを S' にする *)
let initialize_grammar grammar = 
  let (rules, init_nt) = grammar in
  let init_nt' = init_nt ^ prime in
  let rule' = (init_nt', [NT init_nt; T eof]) in
  (rule'::rules, init_nt')

(* tokensの末尾に$を付ける *)
let initialize_tokens tokens = tokens @ [eof]


(** main **)
let parse grammar tokens = 
  let grammar = initialize_grammar grammar in
  let tokens = initialize_tokens tokens in
  let (allrules, init_nt) = grammar in
  let init_state = 
    let init_rules = List.filter (fun (nt,_) -> nt = init_nt) allrules in
    let init_state = List.map (fun rule -> (rule, 0)) init_rules in
    closure init_state allrules
  in
  lr [init_state] [] tokens allrules


(** test **)
let () =
  let rules = [("S", [T "("; NT "L"; T ")"]);
               ("S", [T "x"]);
               ("L", [NT "S"]);
               ("L", [NT "L"; T "*"; NT "S"])] in
  let grammar = (rules, "S") in
  let tokens = ["("; "x"; "*"; "x"; ")"] in
  let ptree = parse grammar tokens in
  print_string "(* parse tree *)";
  print_newline ();
  print_ptree ptree

(* 1. Write a function subset a b that returns true iff (i.e., if and only if)
      a⊆b, i.e., iff the set represented by the list a is a subset of the set 
      represented by the list b. Every set is a subset of itself. This function 
      should be curried, and should be generic to lists of any type: that is, 
      the type of subset should be a generalization of 'a list -> 'a list ->
      bool. *)
let rec subset a b = match a with
  | [] -> true
  | hd::tl -> if List.mem hd b then subset tl b else false;;



(* 2. Write a function equal_sets a b that returns true iff the represented sets * 
     are equal. *)
let equal_sets a b = subset a b && subset b a;;



(* 3. Write a function set_union a b that returns a list representing aUb. *)
let rec set_union a b = match a with
  | [] -> b
  | hd::tl -> if List.mem hd b then set_union tl b else hd::(set_union tl b);;



(* 4. Write a function set_all_union a that returns a list representing U[x∈a]x, 
   i.e., the union of all the members of the set a; a should represent a set of sets. *)
let rec set_all_union a = match a with 
  | [] -> []
  | hd::tl -> set_union hd (set_all_union tl);;



(* 5. Russell's Paradox involves asking whether a set is a member of itself. Write a 
   function self_member s that returns true iff the set represented by s is a member 
   of itself, and explain in a comment why your function is correct; or, if it's not 
   possible to write such a function in OCaml, explain why not in a comment. 
   
   It is not possible to write such a function in OCaml because lists in OCaml are 
   homogenous. Since self_member takes a set s of type 'a list, the elements of s 
   are all expected to be of type 'a. Writing a program to look for an element of 
   type 'a list in an object of type 'a list will not work because of OCaml's 
   type inference functionality and a type mismatch error will be shown. 
 *)



(* 6. Write a function computed_fixed_point eq f x that returns the computed fixed point 
   for f with respect to x, assuming that eq is the equality predicate for f's domain. 
   A common case is that eq will be (=), that is, the builtin equality predicate of OCaml;
   but any predicate can be used. If there is no computed fixed point, your implementation
   can do whatever it wants: for example, it can print a diagnostic, or go into a loop, 
   or send nasty email messages to the user's relatives. *)
let rec computed_fixed_point eq f x = if eq (f x) x then x
                                      else computed_fixed_point eq f (f x);;



(* 7. OK, now for the real work. Write a function filter_reachable g that returns a copy 
   of the grammar g with all unreachable rules removed. This function should preserve 
   the order of rules: that is, all rules that are returned should be in the same order 
   as the rules in g. *)
                                         
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* returns all the nonterminals in a rule's RHS list *)
let rec nt_rhs rhs = match rhs with
  | [] -> []
  | hd::tl -> match hd with
              | N sym -> sym::(nt_rhs tl)
              | _ -> nt_rhs tl;;

(* returns all the nonterminals in a rule list *)
let rec nt_rules rl = match rl with
  | [] -> []
  | hd::tl -> match hd with
              | (lhs, rhs) -> set_union (nt_rhs rhs) (nt_rules tl);;

(* returns start + all the nonterminals reachable from start in a grammar *)
let rec nt_start g = match g with
  | (start, rules) -> start::(nt_rules
                                (List.filter (fun r -> match r with | (lhs,_) -> if lhs = start then true else false) rules));;

(* returns all nonterminals in a rule list that are reachable from a given list of nonterminals *)
let rec nt_indirects nt_list rl = match nt_list with
  | [] -> []
  | hd::tl -> set_union (nt_start (hd, rl)) (nt_indirects tl rl);;

(* set A - set B *)
let rec set_diff a b = match a with
  | [] -> []
  | hd::tl -> if List.mem hd b then set_diff tl b else hd::(set_diff tl b);;
  
(* goes through rule list repeatedly to find all reachable nonterminals *)
let rec nt_all s reachables rl =
  let l = set_diff reachables s
  in match l with
     | [] -> reachables
     | _ -> nt_all reachables (set_union (nt_indirects l rl) reachables) rl;;

(* compiles a list of reachable rules in grammar by looking at list of all reachable nonterminals *)
let rec reachable_grammar g reachables = match g with
  | (start, rules) -> match rules with
                      | [] -> []
                      | hd::tl -> match hd with
                                  | (lhs, rhs) -> if (subset [lhs] reachables) then (lhs, rhs)::(reachable_grammar (start, tl) reachables)
                                                  else reachable_grammar (start, tl) reachables;;

(* main function *)
let filter_reachable g = match g with
  | (start, rules) -> let reachables = nt_all [start] (nt_start g) rules 
     in (start, reachable_grammar g reachables);;                                                                                           

(* symbol type *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* parse tree type *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


          
(* 1. To warm up, notice that the format of grammars is different in this 
   assignment, versus Homework 1. Write a function convert_grammar gram1 that 
   returns a Homework 2-style grammar, which is converted from the Homework 
   1-style grammar gram1. Test your implementation of convert_grammar on the 
   test grammars given in Homework 1. For example, the top-level definition 
   let awksub_grammar_2 = convert_grammar awksub_grammar should bind 
   awksub_grammar_2 to a Homework 2-style grammar that is equivalent to the 
   Homework 1-style grammar awksub_grammar. *)

let convert_grammar gram1 =
  let rec alt_list rules = match rules with
    | [] -> []
    | hd::tl -> match hd with
                | (nt, rhs) -> rhs::(alt_list tl) in
  match gram1 with
  | (start, rules) -> (start, (fun nts -> alt_list (List.filter (fun (n, r) -> n = nts) rules)))


                    
(* 2. As another warmup, write a function parse_tree_leaves tree that traverses 
   the parse tree tree left to right and yields a list of the leaves encountered. *)

let rec parse_tree_leaves tree = 
let rec parse_subtree list =
  match list with
  | [] -> []
  | hd::tl -> match hd with
              | Node (nt, stree) -> (parse_subtree stree) @ (parse_subtree tl)
              | Leaf t -> t::(parse_subtree tl)
in parse_subtree [tree]



(* 3. Write a function make_matcher gram that returns a matcher for the grammar 
   gram. When applied to an acceptor accept and a fragment frag, the matcher must 
   try the grammar rules in order and return the result of calling accept on the 
   suffix corresponding to the first acceptable matching prefix of frag; this is not 
   necessarily the shortest or the longest acceptable match. A match is considered 
   to be acceptable if accept succeeds when given the suffix fragment that immediately 
   follows the matching prefix. When this happens, the matcher returns whatever the 
   acceptor returned. If no acceptable match is found, the matcher returns None. *)

let match_t frag t accept = match frag with
  | hd::tl -> if hd = t
              then accept tl
              else None
  | [] -> None
  
let rec match_frag alt_list prod_fn accept frag = match alt_list with
  | [] -> accept frag
  | hd::tl -> match hd with
              | N nt -> if frag = []
                        then if alt_list = []
                             then accept []
                             else None
                        else match_nt (prod_fn nt) frag prod_fn (match_frag tl prod_fn accept)
              | T t -> match_t frag t (match_frag tl prod_fn accept)
and match_nt alt_list frag prod_fn accept = match alt_list with
  | [] -> None
  | hd::tl -> match (match_frag hd prod_fn accept frag) with
              | None -> match_nt tl frag prod_fn accept
              | Some x -> Some x                                                                 

let make_matcher gram = match gram with
  | (start, prod_fn) -> (fun accept frag -> match_nt (prod_fn start) frag prod_fn accept)



(* 4. Write a function make_parser gram that returns a parser for the grammar gram.
   When applied to a fragment frag, the parser returns an optional parse tree. 
   If frag cannot be parsed entirely (that is, from beginning to end), the parser 
   returns None. Otherwise, it returns Some tree where tree is the parse tree 
   corresponding to the input fragment. Your parser should try grammar rules in 
   the same order as make_matcher. *)

let parse_t frag t accept = match frag with
  | hd::tl -> if hd = t
              then accept tl
              else None
  | [] -> None

let rec parse_frag alt_list accept prod_fn path frag = match alt_list with
  | [] -> accept path frag
  | hd::tl -> match hd with
              | N nt -> if frag = []
                        then if alt_list = []
                             then accept path []
                             else None
                        else parse_nt nt prod_fn (prod_fn nt) (parse_frag tl accept prod_fn) frag path
              | T t -> parse_t frag t (parse_frag tl accept prod_fn path)
and parse_nt start prod_fn alt_list accept frag path = match alt_list with
  | [] -> None
  | hd::tl -> match (parse_frag hd accept prod_fn ((start, hd)::path) frag) with
                     | None -> parse_nt start prod_fn tl accept frag path
                     | Some x -> Some x

let make_path_list gram = 
let p_accept p f = match f with 
  | [] -> Some p
  | _ -> None
in match gram with
  | (start, prod_fn) -> (fun frag -> parse_nt start prod_fn (prod_fn start) p_accept frag [])
                      
let rec make_tree path = match path with
  | hd::tl -> (match make_children tl (snd hd) with
               | (p, c) -> p, Node((fst hd), c))
  | [] -> invalid_arg "path"
and make_children subpath r = match r with
  | [] -> subpath, []
  | hd::tl -> match hd with
              | N nt -> (match (make_tree subpath) with
                         | (p, n) -> (match (make_children p tl) with
                                         | (pp, s) -> pp, n::s))
              | T t -> (match (make_children subpath tl) with
                        | (p, s) -> p, (Leaf t)::s)
    
let make_parser gram frag =
  let l = (make_path_list gram frag)
  in match l with
     | Some path -> Some (snd (make_tree (List.rev path)))
     | None -> None

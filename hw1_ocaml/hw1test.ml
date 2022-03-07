let my_subset_test0 = subset [] [1;2;3;4]
let my_subset_test1 = subset [1;4;3] [1;2;3;4;5]
let my_subset_test2 = not (subset [1;2;3;4] [1;2;3])
let my_subset_test3 = subset [1;2;3] [1;2;3]

let my_equal_sets_test0 = equal_sets [1;2;3] [3;1;2]
let my_equal_sets_test1 = not (equal_sets [] [1])

let my_set_union_test0 = equal_sets (set_union [1;2;3] [3;4;5]) [1;2;3;4;5]
let my_set_union_test1 = equal_sets (set_union [] [1]) [1]
let my_set_union_test2 = equal_sets (set_union [] []) []

let my_set_all_union_test0 = equal_sets (set_all_union [[4;3;5;7];[];[2;3;3;6]]) [4;3;5;7;2;6]
let my_set_all_union_test1 = equal_sets (set_all_union [["a";"b";"c"];["c";"d";"e"];[]]) ["a";"b";"c";"d";"e"]
                           
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x*. 10.) 1. = infinity
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x mod 2) 10 = 0
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x) 3 = 3 

type my_filter_reachable_nonterminals =
  | Knock
  | Hello
  | Bonjour
  | Namaskar
  | Hola
  | Nihao
  | Ahlan

let my_filter_reachable_grammar =
  (Knock,
   [(Nihao, [N Hello; T"knock"; N Ahlan]);
    (Nihao, [N Nihao; N Hola]);
    (Namaskar, [N Ahlan; N Hola]);
    (Hello, [N Ahlan; N Bonjour]);
    (Bonjour, [T"knock"]);
    (Hola, [N Bonjour])])
    
let my_filter_reachable_test0 =
  filter_reachable (Namaskar, (snd my_filter_reachable_grammar)) =
    (Namaskar,
     [Namaskar, [N Ahlan; N Hola];
      Bonjour, [T"knock"];
      Hola, [N Bonjour]])						   

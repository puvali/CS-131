let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type my_nonterminals =
  | Knock
  | Hello
  | GM
  | GN
  | Bye

let my_grammar =
  (Knock,
   function
   | Knock -> [[T"who's there"; N Hello; T"it's me"]]
   | Hello -> [[N Bye; T "?"];
               [T"bedtime"]]
   | GM -> [[N Knock];
            [N Hello; T"how are you"]]
   | GN -> [[T"!"]]
   | Bye -> [[N GN; T"bedtime"];
             [T"sweet dreams"]])

let make_matcher_test = ((make_matcher my_grammar accept_all
                            ["who's there"; "!"; "bedtime"; "?"; "it's me"; "ok"; "!"])
                         = Some ["ok"; "!"])

let my_frag = ["who's there"; "!"; "bedtime"; "?"; "it's me"]

let make_parser_test =
  match make_parser my_grammar my_frag with
  | Some my_tree -> parse_tree_leaves my_tree = my_frag
  | _ -> false                                                                        

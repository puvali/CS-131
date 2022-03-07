%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% kenken/3 using GNU Prolog finite domain solver %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% T is a list of N rows (lists) and each row has N squares
check_row_length([], _).
check_row_length([HD|TL], N) :-
    length(HD, N),
    check_row_length(TL, N).     
check_T_length(T, N) :-
    length(T, N),
    check_row_length(T, N).


% each square can hold an integer from 1 to N
% and every row and column holds all integers from 1 to N
within_domain(N, L) :-
    fd_domain(L, 1, N).
check_unique_1toN(N, L) :-
    fd_all_different(L),
    within_domain(N, L).


% to get the square at row I and column J:
% the Ith element of T is row R
% the Jth element of R is S
square(T, [I|J], S) :-
    nth(I, T, R),
    nth(J, R, S).


% +(S, L) means the integer S is the sum of integers in the list L of squares
sum(_, [], 0). 
sum(T, [HD|TL], S) :-
    square(T, HD, E),
    % E means the square at term HD
    sum(T, TL, SS),
    % SS means sub-sum
    S #= SS + E. 


% *(P, L) means the integer P is the product of the integers in the list L of squares.
product(_, [], 1).
product(T, [HD|TL], P) :-
    square(T, HD, E),
    product(T, TL, SP),
    % SP means sub-product
    P #= SP * E.


% −(D, J, K) means the integer D is the difference between the integer j in
% square J and the integer k in square K; D could be equal to either j−k or to k−j.
difference(T, J, K, D) :-
    square(T, J, E1),
    square(T, K, E2),
    (D #= E1 - E2; D #= E2 - E1).


% /(Q, J, K) means the integer Q is the quotient of the integer j in square J and
% the integer k in square K; Q could be equal to either j÷k or to k÷j. The
% remainder must be zero.
quotient(T, J, K, Q) :-
    square(T, J, E1),
    square(T, K, E2),
    (Q #= E1 / E2; Q #= E2 / E1).


% check cage constraints
check_constraint(T, +(S, L)) :- sum(T, L, S).
check_constraint(T, *(P, L)) :- product(T, L, P).
check_constraint(T, -(D, J, K)) :- difference(T, J, K, D).
check_constraint(T, /(Q, J, K)) :- quotient(T, J, K, Q). 


% transpose - this is from the TAs' hint code repo
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).


% kenken
kenken(N, C, T) :-
    check_T_length(T, N),
    maplist(check_unique_1toN(N), T),
    transpose(T, TT),
    maplist(check_unique_1toN(N), TT),
    maplist(check_constraint(T), C),
    maplist(fd_labeling, T). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plain_kenken/3 without GNU Prolog finite domain solver %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% some of this is from the TAS' hint code repo
plain_make_domain(N, Dm) :-
    findall(X, between(1, N, X), Dm).
plain_within_domain(N, X) :-
    plain_make_domain(N, Dm),
    member(X, Dm).
plain_check_unique(L) :-
    sort(L, Ls),
    length(L, Len),
    length(Ls, LsLen),
    (Len == LsLen).
plain_unique_1toN(N, L) :-
    maplist(plain_within_domain(N), L),
    plain_check_unique(L).


% +(S, L)
plain_sum(_, [], 0).
plain_sum(T, [HD|TL], S) :-
    square(T, HD, E),
    plain_sum(T, TL, SS),
    integer(SS),
    integer(E),
    S is SS + E.


% *(D, L)
plain_product(_, [], 1).
plain_product(T, [HD|TL], P) :-
    square(T, HD, E),
    plain_product(T, TL, SP),
    integer(SP),
    integer(E),
    P is SP * E.


% -(D, J, K, L)
plain_difference(T, J, K, D) :-
    square(T, J, E1),
    square(T, K, E2),
    integer(E1),
    integer(E2),
    (D is E1 - E2 ; D is E2 - E1).


% /(Q, J, K, L)
% !!!
% / is for float division and // is for integer division
plain_quotient(T, J, K, Q) :-
    square(T, J, E1),
    square(T, K, E2),
    integer(E1),
    integer(E2),
    (Q is E1 // E2 ; Q is E2 // E1).


% plain cage constraint checks
plain_check_constraint(T, +(S, L)) :- plain_sum(T, L, S).
plain_check_constraint(T, *(P, L)) :- plain_product(T, L, P).
plain_check_constraint(T, -(D, J, K)) :- plain_difference(T, J, K, D).
plain_check_constraint(T, /(Q, J, K)) :- plain_quotient(T, J, K, Q). 


plain_kenken(N, C, T) :-
    check_T_length(T, N),
    transpose(T, TT),
    maplist(plain_unique_1toN(N), T),
    maplist(plain_unique_1toN(N), TT),
    maplist(plain_check_constraint(T), C).


%%%%%%%%%%%%%%%%%%%%%
%     Testcases     %
%%%%%%%%%%%%%%%%%%%%%


testcase_1(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).
% Result:
% C = [11+[[1|1],[2|1]], /(2,[1|2],[1|3]), 20*[[1|4],[2|4]],
%      6*[[1|5],[1|6],[2|6],[3|6]], -(3,[2|2],[2|3]), /(3,[2|5],[3|5]),
%      240*[[3|1],[3|2],[4|1],[4|2]], 6*[[3|3],[3|4]], 6*[[4|3],[5|3]],
%      7+[[4|4],[5|4],[5|5]], 30*[[4|5],[4|6]], 6*[[5|1],[5|2]],
%      9+[[5|6],[6|6]], 8+[[6|1],[6|2],[6|3]], /(2,[6|4],[6|5])]
% N = 6
% T = [[5,6,3,4,1,2],
%      [6,1,4,5,2,3],
%      [4,5,2,3,6,1],
%      [3,4,1,2,5,6],
%      [2,3,6,1,4,5],
%      [1,2,5,6,3,4]] ? ;
% no


testcase_2(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).
% Result (with write(T), nl, fail):
% [[1,2,3,4],[3,4,2,1],[4,3,1,2],[2,1,4,3]]
% [[1,2,4,3],[3,4,2,1],[4,3,1,2],[2,1,3,4]]
% [[2,1,3,4],[3,4,2,1],[4,3,1,2],[1,2,4,3]]
% [[2,1,4,3],[3,4,2,1],[4,3,1,2],[1,2,3,4]]
% [[3,1,2,4],[2,4,3,1],[4,3,1,2],[1,2,4,3]]
% [[3,2,4,1],[1,4,2,3],[4,3,1,2],[2,1,3,4]]
% no


testcase_3(
	       4,
	       [
		   /(2, [1|1], [1|2]),
		   *(96, [[1|3], [1|4], [2|4], [3|4]]),
		   -(2, [2|1], [2|2]),
		   +(7, [[2|3], [3|3], [4|3], [4|4]]),
		   -(2, [3|1], [4|1]),
		   -(1, [3|2], [4|2])
	       ]
).
% Result:
% C = [/(2,[1|1],[1|2]),
%      96*[[1|3],[1|4],[2|4],[3|4]],
%      -(2,[2|1],[2|2]),
%      7+[[2|3],[3|3],[4|3],[4|4]],
%      -(2,[3|1],[3|4]),
%      -(1,[3|2],[4|2])]
% N = 4
% T = [[1,2,4,3],
%      [3,1,2,4],
%      [4,3,1,2],
%      [2,4,3,1]] ? ;
% no

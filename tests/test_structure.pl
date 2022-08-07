:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(error)).

:- use_module('../src/structure').

% Helper predicates
samsort(Ls0, Ls) :-
        same_length(Ls0, Pairs0),
        pairs_keys(Pairs0, Ls0),
        keysort(Pairs0, Pairs),
        pairs_keys(Pairs, Ls).

% The lambda predicate here has Z = X because of the ordering of 
% arguments in foldl/4, with foldl(Goal, List, Head, End).
none_same(I) :- foldl(\X^Y^Z^(if_(dif(X, Y), Z = X, false)), I, 0, _).

:- must_be(name("some_name"), true).
:- must_be(name("\n"), false).

% No instructions appear twice
:- instr(I), must_be(none_same(I), true).
:- instr(I, _), must_be(none_same(I), true).
:- instr(I, _, _), must_be(none_same(I), true).

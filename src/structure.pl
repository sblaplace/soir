:- use_module(library(si)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).

vec(A) :- length(A, L), L #< 2^32.

% Values

byte(Byte) :- Byte #>= 0x00, 0xFF #>= Byte.

u(N, Number) :-  integer_si(Number), Number #>= 0, 2^N - 1 >= Number.
s(N, Number) :- integer_si(Number), Number #>= -2^(N-1), 2^(N-1)-1 #>= Number.
i(N, Number) :- u(N, Number).

% TODO: Rest of fN
f(N, Number) :- (f(N, Sign, Mag) ; f(N, Sign, Mag)) ; Number.

signif(32, 23).
signif(64, 52).

expon(32, 8).
expon(64, 11).

name(Name) :- 
    length(Name, L), 
    L < 2^32, 
    maplist(\C^(\+ char_type(C, meta), \+ char_type(C, layout)), Name).

% Types

valtype(i(32, N)).
valtype(i(64, N)).
valtype(i(32, N)).
valtype(i(64, N)).

resulttype([]).
resulttype([valtype(_) | resulttype]).

functype(vec(valtype(_)), [valtype(_)]).

limits(min(MinN), max(MaxN)) :- u(32, MinN), u(32, MaxN).

memtype(limits(_Min, _Max)).

tabletype(limits, elemtype).
elemtype(funcref(_, _)).

globaltype(mut(_), valtype(_)).
mut(const).
mut(var).

externtype(func(functype(_))).
externtype(table(tabletype(_))).
externtype(mem(memtype(_))).
externtype(global(globaltype(_))).

% TODO: 2.3.8.1 Conventions
% Filter out specific externtypes in list

% Instructions

nn(32).
nn(64).
sx(u).
sx(s).
instr()
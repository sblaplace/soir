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

valtype(N) :- i(32, N) ; i(64, N) ; f(32, N) ; f(64, N).

resulttype([]).
resulttype([valtype(_) | resulttype]).

functype(vec(valtype(_)), [valtype(_)]).

limits(min(MinN), max(MaxN)) :- u(32, MinN), u(32, MaxN).

memtype(limits(_Min, _Max)).

tabletype(limits, elemtype).
elemtype(funcref(_, _)).

globaltype(mut(_), valtype(_)).
mut(Mut) :- Mut = const ; Mut = var.

externtype(M, T) :- 

% Instructions
:- use_module(library(si)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).

% TODO: Convert to use dcgs and atom_chars/2

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

numtype(i(32, _N)).
numtype(i(64, _N)).
numtype(i(32, _N)).
numtype(i(64, _N)).

vectype(i(128, _N)).

reftype(funcref).
reftype(externref).

valtype(numtype(_)).
valtype(vectype(_)).
valtype(reftype(_)).

resulttype([]).
resulttype([valtype(_) | resulttype]).

functype(resulttype(_), resulttype(_)).

limits(min(u(32, Min)), max(u(32, Max))) :- Max #>= Min.

memtype(limits(_Min, Max)) :- 2^16 #>= Max. 

tabletype(limits(_, Max), elemtype(_)) :- 2^32 #>= Max.
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

nn --> ['32'] | ['64'].
mm --> ['32'] | ['64'].
sx --> [u] | [s].
instr(I) :- phrase(instr_, L), reverse(L, LR), foldl(atom_concat, LR, '', I).
% Memory Instructions /1
instr('memory.size').
instr('memory.grow').
instr('memory.fill').
instr('memory.copy').
% Control Instructions /1
instr(nop).
instr(unreachable).
instr(drop).
instr(select).
instr(return).
% Vector Instructions /1
% Numeric Instructions /2
instr('i32.const', i(nn(32), _)).
instr('i64.const', i(nn(64), _)).
instr('f32.const', i(nn(32), _)).
instr('f64.const', i(nn(64), _)).
% Vector Instructions /2
% i8x16_extract_lane_sx laneidx
instr(I, laneidx(_)) :- sx(SX), atom_concat('i8x16_extract_lane_', SX, I).
% i16x8_extract_lane_sx laneidx
instr(I, laneidx(_)) :- sx(SX), atom_concat('i16x8_extract_lane_', SX, I).
instr('i32x4.extract_lane', laneidx(_)).
instr('i64x2.extract_lane', laneidx(_)).
instr('i8x16.shuffle', laneidx(_)).
% Memory Instructions /2
instr(I, memarg(_)) :- phrase(instr_mem, L), reverse(L, LR), foldl(atom_concat, LR, '', I).
instr('memory.init', dataidx(_)).
instr('data.drop', dataidx(_)).
instr('local.get', localidx(_)).
instr('local.set', localidx(_)).
instr('local.tee', localidx(_)).
instr('global.get', globalidx(_)).
instr('global.set', globalidx(_)).
instr(br, labelidx(_)).
instr(br_if, labelidx(_)).
instr(call, funcidx(_)).
instr(call_indirect, funcidx(_)).
instr(v128_const, i(128, _)).
% Vector Instructions /3
instr(instr_mem_lane, memarg(_), laneidx(_)).
instr(block, resulttype(_), Instrs) :- maplist(instr, Instrs).
instr(loop, resulttype(_), Instrs) :- maplist(instr, Instrs).
instr(br_table, vec(labelidx(_)), labelidx(_)).
instr(if, resulttype(_), Instrs, ElseInstrs) :- maplist(instr, Instrs), maplist(instr, ElseInstrs).
instr_ --> [i], nn, ['.'], (iunop | ibinop | itestop | irelop).
instr_ --> [f], nn, ['.'], (funop | fbinop | frelop).
instr_ --> cvtop | extendN_s.
% inn.load
instr_mem --> ([i] | [f]), nn, (['.load'] | ['.store']).
instr_mem --> ['v128'], (['.load'] | ['.store']).
% inn.load8_sx, inn.load16_sx
instr_mem --> [i], nn, ['.load'], (['8_'] | ['16_']), sx.
% i64.load32_sx
instr_mem --> ['i64.load32_'], sx.
% inn.store8, inn.store16
instr_mem --> ['i'], nn, ['.store'], (['8'] | ['16']).
instr_mem --> ['i64.store32'].
% v128.load8x8_sx
instr_mem --> ['v128.load8x8_'], sx.
instr_mem --> ['v128.load16x4_'], sx.
instr_mem --> ['v128.load32x2_'], sx.
instr_mem --> ['v128.load32_zero'].
instr_mem --> ['v128.load64_zero'].
instr_mem --> ['v128.load'], ww, ['_splat'].
instr_mem_lane --> ['v128'], (['.store'] | ['.load']), ww, ['_lane'].
iunop --> [clz] | [ctz] | [popcnt].
ibinop --> [add] | [sub] | [mul] | (([div_] | [rem_] | [shr_]), sx)
    | [and] | [or] | [xor] | [shl] | [rotl] | [rotr].
funop --> [abs] | [neg] | [sqrt] | [ceil] | [floor] | [trunc] | [nearest].
fbinop --> [and] | [sub] | [mul] | [div] | [min] | [max] | [copysign].
itestop --> [eqz].
irelop --> [eq] | [ne] | (([lt_] | [gt_] | [le_] | [ge_]), sx).
frelop --> [eq] | [ne] | [lt] | [gt] | [le] | [ge].

extendN_s --> [i], nn, ['.extend'], (['8_s'] | ['16_s']).
extendN_s --> ['i64.extend32_s']. 
unop --> iunop | funop | extendN_s.
binop --> ibinop | fbinop.
testop --> itestop.
relop --> irelop | frelop.
cvtop --> ['i32.wrap_i64'].
cvtop --> ['i64.extend_i32_'], sx.
cvtop --> [i], nn, ['.trunc_f'], mm, ['_'], sx.
cvtop --> [i], nn, ['.trunc_sat_f'], mm, ['_'], sx.
cvtop --> ['f32.demote_f64'] | ['f64.promote_f32'].
cvtop --> [f], nn, ['.convert_i'], mm, sx.
cvtop --> [i], nn, ['.reinterpret_f'], nn.
cvtop --> [f], nn, ['.reinterpret_i'], nn.

ishape --> [i8x16] | [i16x8] | [i32x4] | [i64x2].
fshape --> [f32x4] | [f64x2].
shape --> ishape | fshape.
half --> [low] | [high].
laneidx(u(8, _)).

memarg(offset(u(32, _)), align(u(32, _))).
ww --> ['8'] | ['16'] | ['32'] | ['64'].

vvunop --> [not].
vvbinop --> [and] | [andnot] | [or] | [xor].
vvternop --> [bitselect].
vvtestop --> [any_true].
vitestop --> [all_true].
virelop --> [eq] | [ne] | (([lt_] | [gt_] | [le_] | [ge_]), sx).
vfrelop --> [eq] | [ne] | [lt] | [gt] | [le] | [ge].
viunop --> [abs] | [neg].
vibinop --> [add] | [sub].
viminmaxop --> ([min_] | [max_]), sx.
visatbinop --> [shl] | ([shr_], sx).
vfunop --> [abs] | [neg] | [sqrt] | [ceil] | [floor] | [trunc] | [nearest].
vfbinop --> [add] | [sub] | [mul] | [div] | [min] | [max] | [pmin] | [pmax].

expr(Instrs) :- maplist(instr, Instrs).

module(
    types(vec(functype(_))), 
    funcs(vec(func(_))),
    tables(vec(table(_))),
    mems(vec(mem(_))),
    globals(vec(global(_))),
    elem(vec(elem(_))),
    data(vec(data(_))),
    start(start(_)),
    imports(vec(import(_))),
    exports(vec(export(_)))
).

typeidx(u(32, _)).
funcidx(u(32, _)).
tableidx(u(32, _)).
memidx(u(32, _)).
globalidx(u(32, _)).
localidx(u(32, _)).
labelidx(u(32, _)).

func(
    type(typeidx(_)),
    locals(vec(valtype(_))),
    body(expr(_))
).

table(type(tabletype(_, _))).

mem(type(memtype(_))).

global(type(globaltype(_)), init(expr(_))).

elem(table(tableidx(_)), offset(expr(_)), init(vec(funcidx(_)))).

data(data(memidx(_)), offset(expr(_)), init(vec(byte))).

start(func(funcidx(_))).

export(name(name(_)), desc(exportdesc(_))).
exportdesc(func, funcidx(_)).
exportdesc(table, tableidx(_)).
exportdesc(mem, memidx(_)).
exportdesc(global, globalidx(_)).

import(module(name(_)), name(name(_)), desc(importdesc(_))).
importdesc(func, typeidx(_)).
importdesc(table, tabletype(_)).
importdesc(mem, memtype(_)).
importdesc(global, globaltype(_)).
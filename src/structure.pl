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

nn('32').
nn('64').
mm('32').
mm('64').
sx(u).
sx(s).
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
instr('i32.wrap_i64').
instr('f32.demote_f64').
instr('f64.promote_f32').
instr(I) :- sx(SX), atom_concat('i64.extend_i32_', SX, I).
instr(inn_trunc_fmm_sx(nn(_), mm(_), sx(_))).
instr(fnn_convert_imm_sx(nn(_), mm(_), sx(_))).
instr(inn_reinterpret_fnn(nn(_))).
instr(fnn_reinterpret_inn(nn(_))).
instr(inn_const(nn(N), i(nn(N), _))).
instr(fnn_const(nn(N), f(nn(N), _))).
instr(inn_iunop(nn(_), iunop(_))).
instr(fnn_funop(nn(_), funop(_))).
instr(inn_ibinop(nn(_), ibinop(_))).
instr(fnn_fbinop(nn(_), fbinop(_))).
instr(inn_itestop(nn(_), itestop(_))).
instr(inn_irelop(nn(_), irelop(_))).
instr(fnn_frelop(nn(_), frelop(_))).
instr(inn_extendN_s(nn(_), 8)).
instr(inn_extendN_s(nn(_), 16)).
instr(inn_extendN_s(nn(_), 32)).
instr(v128_vvunop).
instr(v128_vvbinop).
instr(v128_vvternop).
instr(v128_vvtestop).
instr(i8x16_swizzle).
instr(shape_splat(shape(_))).
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
instr_ --> [i], [NN], ['.'], (iunop | ibinop | itestop | irelop).
instr_ --> [f], [NN], ['.'], (funop | fbinop | frelop).
instr_ --> cvtop | extendN_s.
% inn.load
instr_mem --> ([i] | [f]), [NN], { nn(NN) }, (['.load'] | ['.store']).
instr_mem --> ['v128'], (['.load'] | ['.store']).
% inn.load8_sx, inn.load16_sx
instr_mem --> [i], [NN], { nn(NN) }, ['.load'], (['8_'] | ['16_']), [SX], { sx(SX) }.
% i64.load32_sx
instr_mem --> ['i64.load32_'], [SX], { sx(SX) }.
% inn.store8, inn.store16
instr_mem --> ['i'], [NN], { nn(NN) }, ['.store'], (['8'] | ['16']).
instr_mem --> ['i64.store32'].
% v128.load8x8_sx
instr_mem --> ['v128.load8x8_'], [SX], { sx(SX) }.
instr_mem --> ['v128.load16x4_'], [SX], { sx(SX) }.
instr_mem --> ['v128.load32x2_'], [SX], { sx(SX) }.
instr_mem --> ['v128.load32_zero'].
instr_mem --> ['v128.load64_zero'].
instr_mem --> ['v128.load'], [WW], { ww(WW) }, ['_splat'].
instr_mem_lane --> ['v128'], (['.store'] | ['.load']), [WW], { ww(WW) }, ['_lane'].
iunop --> [clz] | [ctz] | [popcnt].
ibinop --> [add] | [sub] | [mul] | (([div_] | [rem_] | [shr_]), [SX], { sx(SX) })
    | [and] | [or] | [xor] | [shl] | [rotl] | [rotr].
funop --> [abs] | [neg] | [sqrt] | [ceil] | [floor] | [trunc] | [nearest].
fbinop --> [and] | [sub] | [mul] | [div] | [min] | [max] | [copysign].
itestop --> [eqz].
irelop --> [eq] | [ne] | (([lt_] | [gt_] | [le_] | [ge_]), [SX], { sx(SX) }).
frelop --> [eq] | [ne] | [lt] | [gt] | [le] | [ge].

extendN_s --> [i], [NN], { nn(NN) }, ['.extend'], (['8_s'] | ['16_s']).
extendN_s --> ['i64.extend32_s']. 
unop --> iunop | funop | extendN_s.
binop --> ibinop | fbinop.
testop --> itestop.
relop --> irelop | frelop.
cvtop --> ['i32.wrap_i64'].
cvtop --> ['i64.extend_i32_'], [SX], { sx(SX) }.
cvtop --> [i], [NN], { nn(NN) }, ['.trunc_f'], [MM], { mm(MM) }, ['_'], [SX], { sx(SX) }.
cvtop --> [i], [NN], { nn(NN) }, ['.trunc_sat_f'], [MM], { mm(MM) }, ['_'], [SX], { sx(SX) }.
cvtop --> ['f32.demote_f64'] | ['f64.promote_f32'].
cvtop --> [f], [NN], { nn(NN) }, ['.convert_i'], [MM], { mm(MM) }, [SX], { sx(SX) }.
cvtop --> [i], [NN], { nn(NN) }, ['.reinterpret_f'], [NN], { nn(NN) }.
cvtop --> [f], [NN], { nn(NN) }, ['.reinterpret_i'], [NN], { nn(NN) }.

ishape(i8x16).
ishape(i16x8).
ishape(i32x4).
ishape(i64x2).
fshape(f32x4).
fshape(f64x2).
shape(ishape(_)).
shape(fshape(_)).
half(low).
half(high).
laneidx(u(8, _)).

memarg(offset(u(32, _)), align(u(32, _))).
ww('8').
ww('16').
ww('32').
ww('64').

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
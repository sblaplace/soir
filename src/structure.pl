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

valtype(i(32, _N)).
valtype(i(64, _N)).
valtype(i(32, _N)).
valtype(i(64, _N)).

resulttype([]).
resulttype([valtype(_) | resulttype]).

functype(vec(valtype(_)), [valtype(_)]).

limits(min(MinN), max(MaxN)) :- u(32, MinN), u(32, MaxN).

memtype(limits(_Min, _Max)).

tabletype(limits(_, _), elemtype(_)).
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
mm(32).
mm(64).
sx(u).
sx(s).
instr(inn_const(nn(N), i(nn(N), _))).
instr(fnn_const(nn(N), f(nn(N), _))).
instr(inn_iunop(nn(_), iunop(_))).
instr(fnn_funop(nn(_), funop(_))).
instr(inn_ibinop(nn(_), ibinop(_))).
instr(fnn_fbinop(nn(_), fbinop(_))).
instr(inn_itestop(nn(_), itestop(_))).
instr(inn_irelop(nn(_), irelop(_))).
instr(fnn_frelop(nn(_), frelop(_))).
instr(i32_wrap_i64).
instr(i64_extend_i32_sx(sx(_))).
instr(inn_trunc_fmm_sx(nn(_), mm(_), sx(_))).
instr(f32_demote_f64).
instr(f64_promote_f32).
instr(fnn_convert_imm_sx(nn(_), mm(_), sx(_))).
instr(inn_reinterpret_fnn(nn(_))).
instr(fnn_reinterpret_inn(nn(_))).
iunop(clz).
iunop(ctz).
iunop(popcnt).
ibinop(add).
ibinop(sub).
ibinop(mul).
ibinop(div_sx(sx(_))).
ibinop(rem_sx(sx(_))).
ibinop(and).
ibinop(or).
ibinop(xor).
ibinop(shl).
ibinop(shr_sx(sx(_))).
ibinop(rotl).
ibinop(rotr).
funop(abs).
funop(neg).
funop(sqrt).
funop(ceil).
funop(floor).
funop(trunc).
funop(nearest).
fbinop(add).
fbinop(sub).
fbinop(mul).
fbinop(div).
fbinop(min).
fbinop(max).
fbinop(copysign).
itestop(eqz).
irelop(eq).
irelop(ne).
irelop(lt_sx(sx(_))).
irelop(gt_sx(sx(_))),
irelop(le_sx(sx(_))).
irelop(ge_sx(sx(_))).
frelop(eq).
frelop(ne).
frelop(lt).
frelop(gt).
frelop(le).
frelop(ge).

unop(iunop(_)).
unop(funup(_)).
binop(ibinop(_)).
binop(fbinop(_)).
testop(itestop(_)).
relop(irelop(_)).
relop(frelop(_)).
cvtop(wrap).
cvtop(extend).
cvtop(trunc).
cvtop(convert).
cvtop(demote).
cvtop(promote).
cvtop(reinterpret).

instr(drop).
instr(select).

instr(local_get, localidx(_)).
instr(local_set, localidx(_)).
instr(local_tee, localidx(_)).
instr(global_get, globalidx(_)).
instr(global_set, globalidx(_)).

memarg(offset(u(32, _)), align(u(32, _))).
instr(inn_load(n(_), memarg(_))).
instr(fnn_load(n(_), memarg(_))).
instr(inn_load8_sx(nn(_), sx(_), memarg(_))).
instr(inn_load16_sx(nn(_), sx(_), memarg(_))).
instr(inn_load64_sx(nn(32), sx(_), memarg(_))).
instr(inn_store8_sx(nn(_), sx(_), memarg(_))).
instr(inn_store16_sx(nn(_), sx(_), memarg(_))).
instr(inn_store64_sx(nn(32), sx(_), memarg(_))).
instr(memory_size).
instr(memory_grow).

instr(nop).
instr(unreachable).
instr(block, resulttype(_), Instrs) :- maplist(instr, Instrs).
instr(loop, resulttype(_), Instrs) :- maplist(instr, Instrs).
instr(if, resulttype(_), Instrs, ElseInstrs) :- maplist(instr, Instrs), maplist(instr, ElseInstrs).
instr(br, labelidx(_)).
instr(br_if, labelidx(_)).
instr(br_table, vec(labelidx(_)), labelidx(_)).
instr(return).
instr(call, funcidx(_)).
instr(call_indirect, funcidx(_)).

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
)

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
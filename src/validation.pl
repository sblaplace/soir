:- use_module(library(lists)).
:- use_module('structure.pl').

C(
    types(Types), 
    funcs(Funcs), 
    tables(Tables),
    mems(Mems),
    globals(Globals),
    locals(Locals),
    labels(Labels),
    return(resulttype(_))
) :-
    maplist(functype, Types),
    maplist(functype, Funcs),
    maplist(tabletype, Tables),
    maplist(memtype, Mems),
    maplist(globaltype, Globals),
    maplist(valtype, Locals),
    maplist(labels, Labels).
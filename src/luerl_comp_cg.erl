%% Copyright (c) 2012 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : luerl_comp_cg.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does code generation in the compiler.

-module(luerl_comp_cg).

-include("luerl.hrl").
-include("luerl_comp.hrl").

-export([chunk/2]).

-import(ordsets, [add_element/2,is_element/2,union/1,union/2,
		  subtract/2,intersection/2,new/0]).

%% chunk(St0, Opts) -> {ok,St0}.

chunk(#chunk{code=C0}=St0, Opts) ->
    {Is,St1} = exp(C0, true, St0),
    debug_print(Opts, "cg: ~p\n", [Is]),
    {ok,St1#chunk{code=Is}}.

debug_print(Opts, Format, Args) ->
    case lists:member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

%% push_frame(State) -> State.
%% push_frame(Frame, State) -> State.
%% pop_frame(State) -> State.

push_frame(St) -> push_frame(new_frame(), St).

push_frame(F, #chunk{fs=Fs}=St) ->
    St#chunk{fs=[F|Fs]}.

pop_frame(#chunk{fs=[_|Fs]}=St) ->
    St#chunk{fs=Fs}.

%% new_frame() -> Frame.
%% add_frame_var(Name, Frame) -> Frame.
%% find_frame_var(Name, Frame) -> {yes,Index} | no.
%% fetch_frame_var(Name, Frame) -> Index.
%%  We know frame will be tuples som we index from 1.

new_frame() -> [].

add_frame_var(N, Fs) -> add_frame_var(N, Fs, 1).

add_frame_var(N, [{N,_}|_]=F, _) -> F;		%Already there, reuse it
add_frame_var(N, [V|F], I) ->
    [V|add_frame_var(N, F, I+1)];
add_frame_var(N, [], I) -> [{N,I}].		%Add variable last

find_frame_var(N, [{N,I}|_]) -> {yes,I};
find_frame_var(N, [_|F]) -> find_frame_var(N, F);
find_frame_var(_, []) -> no.

fetch_frame_var(N, [{N,I}|_]) -> I;
fetch_frame_var(N, [_|F]) -> find_frame_var(N, F).

%% add_fs_var(Name, FrameStack) -> FrameStack.
%% find_fs_var(Name, FrameStack) -> {yes,Depth,Index} | no.

add_fs_var(N, [F|Fs]) -> [add_frame_var(N, F)|Fs].

find_fs_var(N, Fs) -> find_fs_var(N, Fs, 1).

find_fs_var(N, [F|Fs], D) ->
    case find_frame_var(N, F) of
	{yes,I} -> {yes,D,I};
	no -> find_fs_var(N, Fs, D+1)
    end;
find_fs_var(_, [], _) -> no.

add_local_var(N, #chunk{locv=true,locf=F0}=St) ->
    F1 = add_frame_var(N, F0),
    St#chunk{locf=F1};
add_local_var(N, #chunk{locv=false,fs=Fs0}=St) ->
    Fs1 = add_fs_var(N, Fs0),
    St#chunk{fs=Fs1}.

get_var(_, _) -> error(boom).

set_var(#lvar{i=I}) -> ?STORE_LVAR(I);
set_var(#fvar{d=D,i=I}) -> ?STORE_FVAR(D, I);
set_var(#gvar{n=N}) -> ?STORE_GVAR(N).

get_var(#lvar{i=I}) -> ?LOAD_LVAR(I);
get_var(#fvar{d=D,i=I}) -> ?LOAD_FVAR(D, I);
get_var(#gvar{n=N}) -> ?LOAD_GVAR(N).

get_frame(#chunk{locv=true,locf=F}) -> F;
get_frame(#chunk{locv=false,fs=[F|_]}) -> F.

%% stat(Stats, State) -> {Istats,State}.

stats([S0|Ss0], St0) ->
    {S1,St1} = stat(S0, nul, St0),
    %% io:format("ss1: ~p\n", [{Loc0,Free0,Used0}]),
    {Ss1,St2} = stats(Ss0, St1),
    {S1 ++ Ss1,St2};
stats([], St) -> {[],St}.

%% stat(Stat, LocalVars, State) -> {Istat,State}.

stat(#assign{vs=Vs,es=Es}, _, St) ->
    assign_stat(Vs, Es, St);
stat(#return{es=Es}=R, _, St0) ->
    {Ies,St1} = explist(Es, false, St0),
    {Ies ++ [?RETURN(length(Es))],St1};
stat(#break{}=B, _, St) -> {B,[],[],St};
stat(#block{}=B, _, St) ->			%Sub-block
    block_stat(B, St);
stat(#while{}=W, _, St) ->
    while_stat(W, St);
stat(#repeat{}=R, _, St) ->
    repeat_stat(R, St);
stat(#'if'{}=I, _, St) ->
    if_stat(I, St);
stat(#nfor{}=F, _, St) ->
    numfor_stat(F, St);
stat(#local{}=L, _, St) ->
    local_stat(L, St);
stat(Exp0, _, St0) ->
    {Exp1,St1} = exp(Exp0, false, St0),
    {Exp1,St1}.

assign_stat(Vs, Es, St) ->
    assign_loop(Vs, Es, St).

%% assign_loop(Vars, Exps, State) -> {Iassigns,State}.
%%  Must be careful with pushing and popping values here. Make sure
%%  all non-last values are singleton.
%%  This could most likely be folded together with assign_local_loop/3.

assign_loop([V], [E], St0) ->			%Remove unnecessary ?PUSH_VALS
    {Ie,St1} = exp(E, true, St0),		%Last argument to one variable
    {Iv,St2} = var(V, St1),
    {Ie ++ Iv,St2};
assign_loop([V|Vs], [E], St0) ->
    {Ie,St1} = exp(E, false, St0),		%Last argument to rest of vars
    {Ias,St2} = assign_loop_var(Vs, 1, St1),
    {Iv,St3} = var(V, St2),
    {Ie ++ Ias ++ Iv,St3};
assign_loop([V|Vs], [E|Es], St0) ->
    {Ie,St1} = exp(E, true, St0),		%Not last argument!
    {Ias,St2} = assign_loop(Vs, Es, St1),
    {Iv,St3} = var(V, St2),
    {Ie ++ [?PUSH] ++ Ias ++ [?POP] ++ Iv,St3};
assign_loop([], Es, St) ->
    assign_loop_exp(Es, St).

assign_loop_var([V|Vs], Vc, St0) ->
    {Ias,St1} = assign_loop_var(Vs, Vc+1, St0),
    {Iv,St2} = var(V, St1),
    {Ias ++ Iv ++ [?POP],St2};
assign_loop_var([], Vc, St) ->
    {[?PUSH_VALS(Vc-1)],St}.			%Last in acc

assign_loop_exp([E|Es], St0) ->
    {Ie,St1} = exp(E, false, St0),		%It will be dropped anyway
    {Ias,St2} = assign_loop_exp(Es, St1),
    {Ie ++ Ias,St2};
assign_loop_exp([], St) -> {[],St}.

var(#dot{e=Exp,r=Rest}, St0) ->
    {Ie,St1} = prefixexp_first(Exp, true, St0),
    {Ir,St2} = var_rest(Rest, St1),
    {[?PUSH] ++ Ie ++ Ir,St2};			%Save acc
var(V, St) ->
    {[set_var(V)],St}.

var_rest(#dot{e=Exp,r=Rest}, St0) ->
    {Ie,St1} = prefixexp_element(Exp, true, St0),
    {Ir,St2} = var_rest(Rest, St1),
    {Ie ++ Ir,St2};
var_rest(Exp, St) -> var_last(Exp, St).

var_last(#key{k=#lit{v=K}}, St) ->
    {[?SET_LIT_KEY(K)],St};			%[?PUSH,?LOAD_LIT(K),?SET_KEY]
var_last(#key{k=Exp}, St0) ->
    {Ie,St1} = exp(Exp, true, St0),
    {[?PUSH] ++ Ie ++ [?SET_KEY],St1}.

%% block_stat(Block, State) -> {Block,State}.

block_stat(Block, St) -> do_block(Block, St).

%% do_block(Block, State) -> {Block,State}.
%%  Do_block never returns external new variables. Fits into stat().

%% do_block(#block{b=Ss,local=[],used=[]}, St) ->	%No local variables in block
%%     stats(Ss, St);				%Fold into surrounding block
do_block(#block{b=Ss,local=Loc,used=U}, St0) ->
    Local = U == [],				%A local stack frame?
    {Ib,St1} = stats(Ss, St0),
    {[?BLOCK(Ib, Local, length(Loc))],St1}.

%% while_stat(While, State) -> {WhileIs,State}.

while_stat(#while{e=E,b=B}, St0) ->
    {Ie,St1} = exp(E, true, St0),
    {Ib,St2} = do_block(B, St1),
    {[?WHILE(Ie, Ib)],St2}.

%% repeat_stat(Repeat, State) -> {RepeatIs,State}.

repeat_stat(#repeat{b=B,e=E}, St0) ->
    {Ib,St1} = do_block(B, St0),
    {Ie,St2} = exp(E, true, St1),
    {[?REPEAT(Ib ++ Ie)],St2}.

%% if_stat(If, State) -> {If,State}.

if_stat(#'if'{tests=Ts,else=E}, St) ->
    if_tests(Ts, E, St).

if_tests([{E,B}|Ts], Else, St0) ->
    {Ie,St1} = exp(E, true, St0),
    {Ib,St2} = do_block(B, St1),
    {Its,St3} = if_tests(Ts, Else, St2),
    {Ie ++ [?IF(Ib, Its)],St3};
if_tests([], Else, St0) ->
    {Ie,St1} = do_block(Else, St0),
    {Ie,St1}.

%% numfor_stat(For, State) -> {ForIs,State}.

numfor_stat(#nfor{v=V,init=I,limit=L,step=S,b=B}, St0) ->
    {Ii,St1} = exp(I, true, St0),
    {Il,St2} = exp(L, true, St1),
    {Is,St3} = exp(S, true, St2),
    {Ib,St4} = do_block(B, St3),
    {[?NFOR(V,Ii,Il,Is,Ib)],St4}.
     
%% local_stat(Local, State) -> {Ilocal,State}.

local_stat(#local{vs=[#var{n=N}],es=[#fdef{}=F0]}=L, St0) ->
    St1 = add_local_var(N, St0),
    {F1,St2} = exp(F0, true, St1),
    {L#local{vs=[get_var(N, St2)],es=[F1]},St2};
local_stat(#local{vs=Vs,es=Es}, St) ->
    assign_local(Vs, Es, St).

assign_local(Vs, [], St0) ->
    {Ias,St1} = assign_local_loop_var(Vs, 0, St0),
    {[?LOAD_LIT(nil)] ++ Ias,St1};
assign_local(Vs, Es, St) ->
    assign_local_loop(Vs, Es, St).

%% assign_local_loop(Vars, Exps, State) -> {Iassigns,State}.
%%  Must be careful with pushing and popping values here. Make sure
%%  all non-last values are singleton.
%%  This could most likely be folded together with assign_loop/3.

assign_local_loop([V], [E], St0) ->		%Remove unnecessary ?PUSH_VALS
    {Ie,St1} = exp(E, true, St0),		%Last argument to one variable!
    {Ie ++ [set_var(V)],St1};
assign_local_loop([V|Vs], [E], St0) ->
    {Ie,St1} = exp(E, false, St0),		%Last argument to many vars!
    {Ias,St2} = assign_local_loop_var(Vs, 1, St1),
    {Ie ++ Ias ++ [set_var(V)],St2};
    %%{Ie ++ [puss1] ++ Ias ++ [popp1,set_var(V)],St2};
assign_local_loop([V|Vs], [E|Es], St0) ->
    {Ie,St1} = exp(E, true, St0),		%Not last argument!
    {Ias,St2} = assign_local_loop(Vs, Es, St1),
    {Ie ++ [?PUSH] ++ Ias ++ [?POP,set_var(V)],St2};
assign_local_loop([], Es, St) ->
    assign_local_loop_exp(Es, St).

assign_local_loop_var([V|Vs], Vc, St0) ->
    {Ias,St1} = assign_local_loop_var(Vs, Vc+1, St0),
    {Ias ++ [set_var(V),?POP],St1};
assign_local_loop_var([], Vc, St) ->
    {[?PUSH_VALS(Vc-1)],St}.			%Last in Acc

assign_local_loop_exp([E|Es], St0) ->
    {Ie,St1} = exp(E, false, St0),		%It will be dropped anyway
    {Ias,St2} = assign_local_loop_exp(Es, St1),
    {Ie ++ Ias,St2};
assign_local_loop_exp([], St) -> {[],St}.

%% explist(Exprs, State) -> {Instrs,State}.
%% explist(Exprs, SingleValue, State) -> {Instrs,State}.
%% exp(Expr, SingleValue, State) -> {Instrs,State}.
%%  Single determines if we are to only return the first value of a
%%  list of values. Single false does not make us a return a list.

explist([E], S, St) -> exp(E, S, St);		%Append values to output?
explist([E|Es], S, St0) ->
    {Ie,St1} = exp(E, true, St0),
    {Ies,St2} = explist(Es, S, St1),
    {Ie ++ [?PUSH] ++ Ies,St2};
explist([], _, St) -> {[],St}.			%No expressions at all

exp(#lit{v=L}, _, St) -> {[?LOAD_LIT(L)],St};
exp(#fdef{}=F, _, St) -> functiondef(F, St);
exp(#op{op='and',as=[A1,A2]}, S, St0) ->
    {Ia1,St1} = exp(A1, true, St0),
    {Ia2,St2} = exp(A2, S, St1),
    {Ia1 ++ [?IF_TRUE(Ia2)],St2};
exp(#op{op='or',as=[A1,A2]}, S, St0) ->
    {Ia1,St1} = exp(A1, true, St0),
    {Ia2,St2} = exp(A2, S, St1),
    {Ia1 ++ [?IF_FALSE(Ia2)],St2};
exp(#op{op=Op,as=As}, S, St0) ->
    {Ias,St1} = explist(As, true, St0),
    Iop = Ias ++ [?OP(Op, length(As))],
    {first_value(S, Iop),St1};
exp(#tc{fs=Fs}, _, St0) ->
    {Its,St1} = tableconstructor(Fs, St0),
    {Its ++ [{build_tab,length(Fs)}],St1};
exp(#lvar{n= <<"...">>}=V, S, St) ->		%Can be either local or frame
    {first_value(S, [get_var(V)]),St};
exp(#fvar{n= <<"...">>}=V, S, St) ->
    {first_value(S, [get_var(V)]),St};
exp(E, S, St) ->
    prefixexp(E, S, St).

first_value(true, Is) -> Is ++ [?SINGLE];
first_value(false, Is) -> Is.

prefixexp(#dot{e=Exp,r=Rest}, S, St0) ->
    {Ie,St1} = prefixexp_first(Exp, true, St0),
    {Ir,St2} = prefixexp_rest(Rest, S, St1),
    {Ie ++ Ir,St2};
prefixexp(Exp, S, St) -> prefixexp_first(Exp, S, St).

prefixexp_first(#single{e=E}, _, St0) ->
    exp(E, true, St0);				%Will make it single
prefixexp_first(Var, _, St) ->
    {[get_var(Var)],St}.

prefixexp_rest(#dot{e=Exp,r=Rest}, S, St0) ->
    {Ie,St1} = prefixexp_element(Exp, true, St0),
    {Ir,St2} = prefixexp_rest(Rest, S, St1),
    {Ie ++ Ir,St2};
prefixexp_rest(Exp, S, St) -> prefixexp_element(Exp, S, St).

prefixexp_element(#key{k=#lit{v=K}}, S, St) ->
    {[?GET_LIT_KEY(K)],St};			%Table is in Acc
prefixexp_element(#key{k=E}, S, St0) ->
    {Ie,St1} = exp(E, true, St0),
    {[?PUSH] ++ Ie ++ [?GET_KEY],St1};		%Table is in Acc
prefixexp_element(#fcall{as=As}, S, St0) ->
    {Ias,St1} = explist(As, false, St0),
    Ifs = [?PUSH] ++ Ias ++ [?FCALL(length(As))],
    {first_value(S, Ifs),St1};			%Function call returns list
prefixexp_element(#mcall{m=#lit{v=K},as=As}, S, St0) ->
    {Ias,St1} = explist(As, false, St0),
    Ims = [?PUSH,				%Push table onto stack
	   ?GET_LIT_KEY(K),			%Get function into acc
	   ?SWAP,				%Swap func onto stack
	   ?PUSH] ++				%Push table as first arg
	Ias ++ [?FCALL(length(As)+1)],
    {first_value(S, Ims),St1}.			%Method call returns list

%% functiondef(Func, State) -> {Func,State}.

functiondef(#fdef{ps=Ps0,b=Ss,local=Loc,used=U}=F, St0) ->
    Local = U == [],				%A local stack frame?
    Ps1 = func_pars(Ps0),
    {Ib,St1} = stats(Ss, St0),
    {[?FDEF(Ps1, Ib, Local, length(Loc))],St1}.

func_pars([#fvar{n= <<"...">>,i=I}]) -> I;	%Tail is index for varargs
func_pars([#lvar{n= <<"...">>,i=I}]) -> I;
func_pars([#fvar{i=I}|Ps]) -> [I|func_pars(Ps)];
func_pars([#lvar{i=I}|Ps]) -> [I|func_pars(Ps)];
func_pars([]) -> [].				%No varargs

%% tableconstructor(Fields, State) -> {Ifields,State}.

tableconstructor(Fs0, St0) ->
     Fun = fun (#efield{v=V}, {Ifs,I,S0}) ->
		  {Iv,S1} = exp(V, true, S0),
		  {Ifs ++ [?LOAD_LIT(I),?PUSH] ++ Iv ++ [?PUSH],I+1,S1};
	      (#kfield{k=K,v=V}, {Ifs,I,S0}) ->
		  {Ik,S1} = exp(K, true, S0),
		  {Iv,S2} = exp(V, true, S1),
		  {Ifs ++ Ik ++ [?PUSH] ++ Iv ++ [?PUSH],I,S2}
	  end,
    {Its,_,St1} = lists:foldl(Fun, {[],1.0,St0}, Fs0),
    {Its,St1}.

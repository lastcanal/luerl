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

%% File    : luerl_comp.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% This is the main loop of the Luerl compiler. While we can handle
%% errors in this loop they should never occur as Lua basically allows
%% almost everything that gets past the parser. The only exception are
%% goto's to undefined labels, but we don't handle goto's yet.
%%
%% We also have the first pass here. It normalises the code and
%% converts to an internal form.

-module(luerl_comp).

-export([file/1,file/2,string/1,string/2,forms/1,forms/2]).

-include("luerl.hrl").
-include("luerl_comp.hrl").

-record(comp, {lfile="",			%Lua file
	       code=none,			%Code/chunk
	       opts=[],				%Options
	       errors=[],			%Errors
	       warnings=[]			%Warnings
	      }).

file(Name) -> file(Name, [verbose,report]).

file(Name, Opts) ->
    Cst = #comp{lfile=Name,opts=Opts},
    compile(file_passes(), Cst).

string(Str) -> string(Str, [verbose,report]).

string(Str, Opts) when is_binary(Str) ->
    string(binary_to_list(Str), Opts);
string(Str, Opts) when is_list(Str) ->
    Cst = #comp{code=Str,opts=Opts},
    compile(list_passes(), Cst).

forms(Forms) -> forms(Forms, [verbose,report]).

forms(Forms, Opts) ->
    Cst = #comp{code=Forms,opts=Opts},
    compile(forms_passes(), Cst).

compile(Ps, Cst0) ->
    case do_passes(Ps, Cst0) of
	{ok,Cst1} -> do_ok_return(Cst1);
	{error, Cst1} -> do_error_return(Cst1)
    end.

%% file_passes() -> [Pass].
%% list_passes() -> [Pass].
%% forms_passes() -> [Pass].
%% do_passes(Passes, State) -> {ok,State} | {error,Reason}.
%%  {when_flag,Flag,Cmd}
%%  {unless_flag,Flag,Cmd}
%%  {do,Fun}
%%  {done,PrintFun,Ext}

file_passes() ->				%Reading from file
    [{do,fun do_read_file/1}|
     list_passes()].

list_passes() ->				%Scanning string
    [{do,fun do_scan/1},
     {do,fun do_parse/1}|
     forms_passes()].

forms_passes() ->				%Doing the forms
    [{do,fun do_pass_1/1},
     {do,fun do_pass_2/1},
     {do,fun do_pass_3/1},
     {do,fun do_code_gen/1},
     {unless_flag,no_iopt,{do,fun do_peep_op/1}}].

do_passes([{do,Fun}|Ps], Cst0) ->
    case Fun(Cst0) of
	{ok,Cst1} -> do_passes(Ps, Cst1); 
	{error,Cst1} -> {error,Cst1}
    end;
do_passes([{when_flag,Flag,Cmd}|Ps], Cst) ->
    case lists:member(Flag, Cst#comp.opts) of
	true -> do_passes([Cmd|Ps], Cst);
	false -> do_passes(Ps, Cst)
    end;
do_passes([{unless_flag,Flag,Cmd}|Ps], Cst) ->
    case lists:member(Flag, Cst#comp.opts) of
	true -> do_passes(Ps, Cst);
	false -> do_passes([Cmd|Ps], Cst)
    end;
do_passes([{done,Fun}|_], Cst) ->
    Fun(Cst);
do_passes([], Cst) -> {ok,Cst}.

%% do_read_file(State) -> {ok,State} | {error,State}.
%% do_scan(State) -> {ok,State} | {error,State}.
%% do_parse(State) -> {ok,State} | {error,State}.
%% do_pass_1(State) -> {ok,State} | {error,State}.
%% do_return(State) -> {ok,State}.
%%  The actual compiler passes.

do_read_file(#comp{lfile=Name}=Cst) ->
    case file:read_file(Name) of
	{ok,Bin} -> {ok,Cst#comp{code=binary_to_list(Bin)}}; 
	{error,E} -> {error,Cst#comp{errors=[E]}}
    end.

do_scan(#comp{code=Str}=Cst) ->
    case luerl_scan:string(Str) of
	{ok,Ts,_} -> {ok,Cst#comp{code=Ts}}; 
	{error,E,_} -> {error,Cst#comp{errors=[E]}}
    end.

do_parse(#comp{code=Ts}=Cst) ->
    case luerl_parse:chunk(Ts) of
	{ok,Chunk} -> {ok,Cst#comp{code=Chunk}};
	{error,E} -> {error,Cst#comp{errors=[E]}}
    end.

do_pass_1(#comp{code=C0,opts=Opts}=Cst) ->
    {ok,C1} = chunk(C0, Opts),
    {ok,Cst#comp{code=C1}}.

do_pass_2(Cst) ->
    case luerl_comp_vars:chunk(Cst#comp.code, Cst#comp.opts) of
	{ok,C1} -> {ok,Cst#comp{code=C1}};
	{ok,C1,Ws} -> {ok,Cst#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,Cst#comp{errors=Es}}
    end.

do_pass_3(Cst) ->
    case luerl_comp_env:chunk(Cst#comp.code, Cst#comp.opts) of
	{ok,C1} -> {ok,Cst#comp{code=C1}};
	{ok,C1,Ws} -> {ok,Cst#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,Cst#comp{errors=Es}}
    end.

do_code_gen(Cst) ->
    case luerl_comp_cg:chunk(Cst#comp.code, Cst#comp.opts) of
	{ok,C1} -> {ok,Cst#comp{code=C1}};
	{ok,C1,Ws} -> {ok,Cst#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,Cst#comp{errors=Es}}
    end.

do_peep_op(Cst) ->
    case luerl_comp_peep:chunk(Cst#comp.code, Cst#comp.opts) of
	{ok,C1} -> {ok,Cst#comp{code=C1}};
	{ok,C1,Ws} -> {ok,Cst#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,Cst#comp{errors=Es}}
    end.

do_ok_return(#comp{code=C}) -> {ok,C}.

do_error_return(#comp{errors=Es,warnings=Ws}) ->
    {error,Es,Ws}.

%% chunk(Code, Options) -> {ok,Code} | {error,Reason}.

chunk(Code, Opts) ->
    St0 = #chunk{code=Code},			%Initialise internal state
    {C1,St1} = exp(Code, St0),			%This is local!
    debug_print(Opts, "c: ~p\n", [C1]),
    {ok,St1#chunk{code=C1}}.

debug_print(Opts, Format, Args) ->
    case lists:member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

%% The first pass (pass_1).
%% Here we normalise the code and convert it to an internal form. 

stats([{';',_}|Ss], St) -> stats(Ss, St);	%No-op so we drop it
stats([S0|Ss0], St0) ->
    {S1,St1} = stat(S0, St0),
    {Ss1,St2} = stats(Ss0, St1),
    {[S1|Ss1],St2};
stats([], St) -> {[],St}.

%% stat(Statement, State) -> {CStat,State}.
%%  Do a statement. The ';' statement will caught and removed in stats/2.

stat({assign,L,Vs,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {#assign{l=L,vs=Cvs,es=Ces},St2};
stat({return,L,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {#return{l=L,es=Ces},St1};
stat({break,L}, St) ->				%Interesting
    {#break{l=L},St};
stat({block,L,B}, St0) ->
    {Cb,St1} = block(L, B, St0),
    {Cb,St1};
stat({while,L,Exp,B}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Cb,St2} = block(L, B, St1),
    {#while{l=L,e=Ce,b=Cb},St2};
stat({repeat,L,B,Exp}, St0) ->
    {Cb,St1} = block(L, B, St0),
    {Ce,St2} = exp(Exp, St1),
    {#repeat{l=L,b=Cb,e=Ce},St2};
stat({'if',Line,Tests,Else}, St0) ->
    {Cts,St1} = if_tests(Line, Tests, St0),
    {Ce,St2} = block(Line, Else, St1),
    {#'if'{l=Line,tests=Cts,else=Ce},St2};
stat({for,Line,V0,I0,L0,B0}, St0) ->		%Default step of 1.0
    {V1,I1,L1,S1,B1,St1} = numeric_for(Line, V0, I0, L0, {'NUMBER',Line,1.0},
				       B0, St0),
    {#nfor{l=Line,v=V1,init=I1,limit=L1,step=S1,b=B1},St1};
stat({for,Line,V0,I0,L0,S0,B0}, St0) ->
    {V1,I1,L1,S1,B1,St1} = numeric_for(Line, V0, I0, L0, S0, B0, St0),
    {#nfor{l=Line,v=V1,init=I1,limit=L1,step=S1,b=B1},St1};
stat({for,Line,Ns0,Gs0,B0}, St0) ->
    {Ns1,Gs1,B1,St1} = generic_for(Line, Ns0, Gs0, B0, St0),
    {#gfor{l=Line,vs=Ns1,gens=Gs1,b=B1},St1};
stat({functiondef,L,Fname,Ps,B}, St0) ->
    {V,F,St1} = functiondef(L, Fname, Ps, B, St0),
    {#assign{l=L,vs=[V],es=[F]},St1};
stat({local,L,Local}, St0) ->
    {Vs,Es,St1} = local(Local, St0),
    {#local{l=L,vs=Vs,es=Es},St1};
stat(Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Ce,St1}.

assign_loop([V|Vs], St0) ->
    {Cv,St1} = var(V, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {[Cv|Cvs],St2};
assign_loop([], St) -> {[],St}.

%% var(VarExp, State) -> {VarExp,State}.
%%  Step down the prefixexp sequence evaluating as we go, stop at the
%%  END and return a key and a table where to put data. This is a
%%  prefixexp with different tail.

var({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var({'NAME',L,N}, St) -> {var_name(L, N),St}.

var_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string. NO!
    {#key{l=L,k=lit_name(L, N)},St};
var_last({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{l=L,k=Ce},St1}.

if_tests(L, Ts, St) ->
    Test = fun ({T,B}, S0) ->
		   {Ct,S1} = exp(T, S0),
		   {Cb,S2} = block(L, B, S1),
		   {{Ct,Cb},S2}
	   end,
    lists:mapfoldl(Test, St, Ts).

%% numeric_for(Line, Var, Init, Limit, Step, Stats, State) ->
%%     (Var, Init, Limit, Step, Block, State).

numeric_for(L, {'NAME',Ln,N}, I0, L0, S0, Ss, St0) ->
    Var = var_name(Ln, N),
    {[I1,L1,S1],St1} = explist([I0,L0,S0], St0),
    {B,St2} = block(L, Ss, St1),
    {Var,I1,L1,S1,B,St2}.

%% generic_for(Line, Vars, Generators, Stats, State) ->
%%     (Vars, Generators, Block, State).

generic_for(L, Vs0, Gs0, Ss, St0) ->
    Vs1 = [ var_name(Ln, N) || {'NAME',Ln,N} <- Vs0 ],
    {Gs1,St1} = explist(Gs0, St0),
    {B,St2} = block(L, Ss, St1),
    {Vs1,Gs1,B,St2}.

block(L, Ss0, St0) ->
    {Ss1,St1} = stats(Ss0, St0),
    {#block{l=L,b=Ss1},St1}.

%% functiondef(Line, Pars, Block, State) -> {CFunc,State}.
%% functiondef(Line, Name, Pars, Block, State) -> {Var,CFunc,State}.
%%  Have to handle the case where the function is a "method". All this
%%  really means is that the function has an extra parameter 'self'
%%  prepended to the paramter list.

functiondef(L, Ps, B, St0) ->
    {Cp,Cb,St1} = function_block(Ps, B, St0),
    {#fdef{l=L,ps=Cp,b=Cb},St1}.

functiondef(L, Name0, Ps0, B, St0) ->
    %% Check if method and transform method to 'NAME'.
    case is_method(Name0) of			%Export Name1 and Ps1
	{yes,Name1} -> Ps1 = [{'NAME',L,self}|Ps0];
	no -> Name1 = Name0, Ps1 = Ps0
    end,
    {Var,St1} = funcname(Name1, St0),
    {F,St2} = functiondef(L, Ps1, B, St1),
    {Var,F,St2}.

is_method({'NAME',_,_}) -> no;
is_method({'.',L,N,Rest0}) ->
    case is_method(Rest0) of
        {yes,Rest1} -> {yes,{'.',L,N,Rest1}};
        no -> no                                %No change
    end;
is_method({method,_,{'NAME',_,_}=N}) -> {yes,N}.

%% funcname(FuncNameExp, State) -> {CFuncNameExp,State}.

funcname({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_first(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
funcname({'NAME',L,N}, St) ->
    {var_name(L, N),St}.

funcname_first({'NAME',L,N}, St) ->
    {var_name(L, N),St}.

funcname_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_element(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
funcname_rest(Exp, St) ->
    funcname_last(Exp, St).

funcname_element({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{l=L,k=lit_name(L, N)},St}.

%% Method call has been transformed away
funcname_last({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{l=L,k=lit_name(L, N)},St}.

%% local(Local, State) -> {Vars,Values,State}.
%%  Create and assign local variables.

local({assign,_,Ns,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cns,St2} = lists:mapfoldl(fun (V, St) -> var(V, St) end, St1, Ns),
    {Cns,Ces,St2};
local({functiondef,Lf,Name,Ps,B}, St0) ->
    %% Name is really only a variable here.
    {V,F,St1} = functiondef(Lf, Name, Ps, B, St0),
    {[V],[F],St1}.

explist([E|Es], St0) ->
    {Ce,St1} = exp(E, St0),
    {Ces,St2} = explist(Es, St1),
    {[Ce|Ces],St2};
explist([], St) -> {[],St}.			%No expressions at all

%% exp(Expression, State) -> {Ins,State}.

exp({nil,L}, St) -> {#lit{l=L,v=nil},St};
exp({false,L}, St) -> {#lit{l=L,v=false},St};
exp({true,L}, St) -> {#lit{l=L,v=true},St};
exp({'NUMBER',L,N}, St) -> {#lit{l=L,v=N},St};
exp({'STRING',L,S}, St) -> {#lit{l=L,v=S},St};
exp({'...',L}, St) ->
    {var_name(L, '...'),St};
    %% {#lit{l=L,v='...'},St};
exp({functiondef,L,Ps,B}, St0) ->
    {Cf,St1} = functiondef(L, Ps, B, St0),
    {Cf,St1};
exp({table,L,Fs}, St0) ->
    {Cfs,St1} = tableconstructor(Fs, St0),
    {#tc{l=L,fs=Cfs},St1};
exp({op,L,Op,A1,A2}, St0) ->
    {Ca1,St1} = exp(A1, St0),
    {Ca2,St2} = exp(A2, St1),
    {#op{l=L,op=Op,as=[Ca1,Ca2]},St2};
exp({op,L,Op,A}, St0) ->
    {Ca,St1} = exp(A, St0),
    {#op{l=L,op=Op,as=[Ca]},St1};
exp(E, St) ->
    prefixexp(E, St).

%% prefixexp(PrefixExp, State) -> {CPrefixExp,State}.

prefixexp({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
prefixexp(P, St) -> prefixexp_first(P, St).

prefixexp_first({'NAME',L,N}, St) ->
    {var_name(L, N),St};
prefixexp_first({single,L,E}, St0) ->
    {Ce,St1} = exp(E, St0),
    {#single{l=L,e=Ce},St1}.

prefixexp_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string
    {#key{l=L,k=lit_name(L, N)},St};
prefixexp_element({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{l=L,k=Ce},St1};
prefixexp_element({functioncall,L,Args}, St0) ->
    {Cas,St1} = explist(Args, St0),
    {#fcall{l=L,as=Cas},St1};
prefixexp_element({method,Lm,{'NAME',Ln,N},Args}, St0) ->
    {Args1,St1} = explist(Args, St0),
    {#mcall{l=Lm,m=lit_name(Ln, N),as=Args1},St1}.

dot(L, Exp, Rest) -> #dot{l=L,e=Exp,r=Rest}.

function_block(Pars, Stats, St0)->
    {Cps,St1} = make_local_pars(Pars, St0),
    {Cs,St2} = stats(Stats, St1),
    %% io:format("fb: ~p\n", [{St3#comp.fs}]),
    {Cps,Cs,St2}.

make_local_pars(Ps, St) ->
    Add = fun ({'NAME',L,N}, S) -> {var_name(L, N),S};
	      ({'...',L}, S) -> {var_name(L, '...'),S}
	  end,
    lists:mapfoldl(Add, St, Ps).

%% tableconstrutor(Fields, State) -> {Instrs,State}.
%%  Build the instructions to construct a table. We could be smarter
%%  here and recognise already uses keys and only actually insert the
%%  last one. Or we could pre-order the table elements so the keys are
%%  already sorted. We can't unpack the last field if it is a multiple
%%  value efield as this must be done at run-time.

tableconstructor(Fs, St0) ->
    %% N.B. this fun is for a MAPFOLDL!!
    Fun = fun ({exp_field,L,Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  {#efield{l=L,v=Ce},S1};
	      ({name_field,L,{'NAME',Ln,N},Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  {#kfield{l=L,k=lit_name(Ln, N),v=Ce},S1};
	      ({key_field,L,Ke,Ve}, S0) ->
		  {Ck,S1} = exp(Ke, S0),	%Key
		  {Cv,S2} = exp(Ve, S1),	%Value
		  {#kfield{l=L,k=Ck,v=Cv},S2}
	  end,
    {Cfs,St1} = lists:mapfoldl(Fun, St0, Fs),
    {Cfs,St1}.

%% name_string(Name) -> String.
%% var_name(Line, Name) -> #var{}.
%% lit_name(Line, Name) -> #lit{}.

name_string(Name) -> atom_to_binary(Name, latin1).

lit_name(L, N) -> #lit{l=L,v=name_string(N)}.

var_name(L, N) -> #var{l=L,n=name_string(N)}.

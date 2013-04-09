-module(hello_mfa).
-export([run/0, fun_factory/1]).

%% returns a function that is callable by Luerl
fun_factory(Outer) ->
  fun([Inner], LuaState) ->
    % check out process from Pool
    % call ihttpc request
    % recieve response
    {[Outer, Inner], LuaState}
  end.

run() ->
  %% Init the luerl state
  Lua0 = luerl:init(),

  %% Create a new empty table
  {HttpTable, Lua1} = luerl_emul:alloc_table(Lua0),

  %% Set the empty table to the global key `http`
  Lua2 = luerl_emul:set_global_key(<<"http">>, HttpTable, Lua1),

  %% You can also set up the global table from Lua
  % {[HttpTable], Lua2} = luerl:do("http = { request = nil }; return http"),

  %% Path to http request. Translates to 'http.request' in Lua
  FunctionPath = [<<"http">>, <<"request">>],

  %% This module function must return function that accepts a
  %% list as the first arguement and a luerl state as the second 
  Value = {mfa, ?MODULE, fun_factory, [<<"outer">>]},

  %% Set the table in the lua state
  {HttpTable, Lua3} = luerl:set_table1(FunctionPath, Value, Lua2),

  %% Call the function from Erlang
  {Resp, _Lua5} = luerl:call_function1(FunctionPath, [<<"inner">>], Lua3),
  io:format("(1) ~p~n", [Resp]),

  %% Or call the function from Lua
  {Resp, _Lua7} = luerl:do("return http.request('inner')", Lua3),
  io:format("(2) ~p~n", [Resp]),

  [<<"outer">>, <<"inner">>] = Resp.


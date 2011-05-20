%% @author Oleg Krivosheev <amranello@gmail.com>
%% @copyright 2010 Oleg Krivosheev

%% Copyright 2010 Oleg Krivosheev
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module('orologio-node_spawn').

-include("include/orologio-node.hrl").

-export([start/1]).

start({PName, Elem, Env}) ->
  process_flag(trap_exit, true),
  Agent = filename:absname(filename:join([orologio_utils:get_opt(agents_dir, "agents"), orologio_utils:check_str(Elem)])),
  Port = open_port({spawn_executable, Agent}, [binary, in, {line, ?PORT_LINE}, {env, Env}]),
  loop({PName, Port, [], []}).

loop({PName, Port, Msgs, TrMsgs}) ->
  receive
    {_, {data, {eol, Msg}}} when length(TrMsgs) == 0 ->
      loop({PName, Port, [Msg|Msgs], []});
    {_, {data, {eol, Msg}}} ->
      Ret = lists:map(fun(El) -> binary_to_list(El) end, lists:reverse([Msg|TrMsgs])),
      loop({PName, Port, [list_to_binary(lists:flatten(Ret))|Msgs], []});
    {_, {data, {noeol, Msg}}} ->
      loop({PName, Port, Msgs, [Msg|TrMsgs]});
    {'EXIT', _Pt, _Res} ->
      PName ! {data_spawn, lists:reverse(Msgs), now()};
    Dat ->
      PName ! {data_spawn_oth, {error, Dat}},
      Port ! {self(), close}
  end.
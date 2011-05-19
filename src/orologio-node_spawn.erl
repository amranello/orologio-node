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
-module('orologio-node_app').
-behaviour(application).
-export([start/2, stop/1, config_change/3]).

-include("include/orologio-node.hrl").

start(_Typ, _Args) ->
  Log = orologio_utils:get_opt(mod_log, mod_log_file),
  Log:open(orologio_utils:parse_term(orologio_utils:get_opt(log, "orologio-node"), orologio_utils:gen_time_fmt(now()))),
  Ret = orologio_sup:start_link([]),
  orologio_utils:log_report(info, [{?MODULE, "Started"}]),
  Ret.

stop(_State) ->
  orologio_utils:log_report(info, [{?MODULE, "Stopped"}]),
  Log = orologio_utils:get_opt(mod_log, mod_log_file),
  Log:close(),
  ok.

config_change(_Changed, _New, _Removed) ->
  ok.
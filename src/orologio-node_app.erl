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
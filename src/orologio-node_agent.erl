-module('orologio-node_agent').
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/orologio-node.hrl").

-record(agent_state, {host, elem, port, tag}).

start_link({Host, Elem}) ->
  gen_server:start_link(?MODULE, {Host, Elem}, []).

init({Host, Elem}) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), {init, <<"Started">>}),
  {ok, #agent_state{host = Host, elem = Elem}}.

handle_call(stop, _From, State) ->
  {stop, stop, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({init, Act}, State) ->
  Host = State#agent_state.host,
  Elem = State#agent_state.elem,
  Cfg = orologio_utils:get_conf_all([Host, Elem], {agent, []}),
  File = filename:absname(filename:join([proplists:get_value(dir, Cfg, "agents"),
                                         orologio_utils:check_str(Elem)]
                                       )
                         ),
  Env = proplists:get_value(env, Cfg, ""),
  Tag = proplists:get_value(tag, Cfg, update),
  Port = open_port({spawn_executable, File}, [binary, in, {line, ?PORT_LINE}, {env, Env}]),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({fetch, State#agent_state.host, State#agent_state.elem}),
                                   Act}]),
  {noreply, State#agent_state{port = Port, tag = Tag}};

handle_cast(recfg, State) ->
  catch port_close(State#agent_state.port),
  handle_cast({init, <<"Config reloaded">>}, State);

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({_Port, {data, {_Eol, Data}}}, State) ->
  catch orologio_utils:event(send, {{limit, State#agent_state.host, State#agent_state.elem},
                                     {State#agent_state.tag, Data, now()}}),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  catch port_close(State#agent_state.port),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({agent, State#agent_state.host, State#agent_state.elem}),
                                    <<"Stoped">>}]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
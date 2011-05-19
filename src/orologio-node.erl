-module('orologio-node').
-export([start/0, stop/0]).

start() ->
  application:start('orologio-node').
  
stop() ->
  application:stop('orologio-node').
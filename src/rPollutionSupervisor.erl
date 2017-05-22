%%%-------------------------------------------------------------------
%%% @author Anna
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2017 12:36
%%%-------------------------------------------------------------------
-module(rPollutionSupervisor).
-author("Anna").
-behavior(supervisor).

%% API
-export([start_link/0, init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, 'Pollution':createMonitor()).


init(_Args) ->
  {ok, {
    {one_for_one, 2, 2000},
    [rPollutionServer, {rPollutionServer, start_link(), []},
      permanent, brutal_kill, worker, [rPollutionServer]]
  }}.
%%%-------------------------------------------------------------------
%%% @author Anna
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2017 11:36
%%%-------------------------------------------------------------------
-module(rPollutionServer).
-author("Anna").
-behavior(gen_server).

%% API
-export([start_link/0, init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([addStation/2, close/0, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, exportToJSON/0]).

%START
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,'Pollution':createMonitor(),[]).

%INIT
init(Monitor) ->
  {ok, Monitor}.

%API
addStation(StationName, Coordinates) ->
  gen_server:cast(?MODULE, {addStation, StationName, Coordinates}).
addValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value) ->
  gen_server:cast(?MODULE, {addValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value}).
removeValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type) ->
  gen_server:cast(?MODULE, {removeValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type}).
getOneValue(Station, Date, Type) ->
  gen_server:call(?MODULE, {getOneValue, Station, Date, Type}).
getStationMean(Type, Station) ->
  gen_server:call(?MODULE, {getStationMean, Station, Type}).
getDailyMean(Type, {Year, Month, Day}) ->
  gen_server:call(?MODULE, {getDailyMean, Type, {Year, Month, Day}}).
exportToJSON() ->
  gen_server:call(?MODULE, {exportToJSON}).

close() ->
  gen_server:call(?MODULE, terminate).


%HANDLE MESSAGES
handle_cast({addStation, StationName, Coordinates}, Monitor) ->
  NewMonitor = controlResult(Monitor, 'Pollution':addStation(StationName, Coordinates, Monitor)),
  {noreply, NewMonitor};
handle_cast({addValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value}, Monitor) ->
  NewMonitor = controlResult(Monitor, 'Pollution':addValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, Monitor)),
  {noreply, NewMonitor};
handle_cast({removeValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type}, Monitor) ->
  NewMonitor = controlResult(Monitor, 'Pollution':removeValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Monitor)),
  {noreply, NewMonitor}.


handle_call(terminate, _From, Monitor) ->
  {stop, normal, ok, Monitor};
handle_call({getOneValue, Station, Date, Type}, _From, Monitor) ->
  NewMonitor = controlGetting(Monitor, 'Pollution':getOneValue(Date, Type, Station, Monitor)),
  {reply, NewMonitor, NewMonitor};
handle_call({getStationMean, Station, Type}, _From, Monitor) ->
  NewMonitor = controlGetting(Monitor, 'Pollution':getStationMean(Type, Station, Monitor)),
  {reply, NewMonitor, NewMonitor};
handle_call({getDailyMean, Type, Date}, _From, Monitor) ->
  NewMonitor = controlGetting(Monitor, 'Pollution':getDailyMean(Type, Date, Monitor)),
  {reply, NewMonitor, NewMonitor};
handle_call({exportToJSON}, _From, Monitor) ->
  NewMonitor = controlExport(Monitor, 'Pollution':exportToJSON(Monitor)),
  {reply, NewMonitor, NewMonitor}.


controlGetting(Monitor, {error, Description}) ->
  io:format("<Error> ~s~n", [Description]),
  Monitor;
controlGetting(Monitor, V) ->
  io:format("~w~n", [V]),
  Monitor.

controlExport(Monitor, {error, Description}) ->
  io:format("<Error> ~s~n", [Description]),
  Monitor;
controlExport(Monitor, V) ->
  io:format("~s~n", [V]),
  Monitor.

controlResult(Monitor, {error, Description}) ->
  io:format("<Error> ~s~n", [Description]),
  Monitor;
controlResult(_, NewMonitor) ->
  io:format("~w~n", [NewMonitor]),
  NewMonitor.


%END
terminate(normal, Monitor) ->
  io:format("Monitor: ~w.~nGoodbye.~n", [Monitor]), ok.


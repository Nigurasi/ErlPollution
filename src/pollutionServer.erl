%%%-------------------------------------------------------------------
%%% @author Anna
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. maj 2017 20:05
%%%-------------------------------------------------------------------
-module(pollutionServer).
-author("Anna").

%% API
-export([start/0, init/0, addStation/2, addValue/4, getOneValue/3, removeValue/3, getStationMean/2, getDailyMean/2, exportToJson/0]).

start() ->
  register(pollutionServer, spawn_link(?MODULE, init, [])).

init() ->
  loop('Pollution':createMonitor()).

addStation(StationName, Coordinates) ->
  pollutionServer ! {addStation, StationName, Coordinates, self()},
  receiveAnswer().

addValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value) ->
  pollutionServer ! {addValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, self()},
  receiveAnswer().

removeValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type) ->
  pollutionServer ! {removeValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, self()},
  receiveAnswer().

getOneValue(Date, Type, Station) ->
  pollutionServer ! {getOneValue, Date, Type, Station, self()},
  receiveAnswer().

getStationMean(Type, Station) ->
  pollutionServer ! {getStationMean, Type, Station, self()},
  receiveAnswer().

getDailyMean(Type, {Year, Month, Day}) ->
  pollutionServer ! {getDailyMean, Type, {Year, Month, Day}, self()},
  receiveAnswer().

exportToJson() ->
  pollutionServer ! {exportToJson}.

loop(Monitor) ->
  receive
    {addStation, StationName, Coordinates, Pid} ->
      NewMonitor = 'Pollution':addStation(StationName, Coordinates, Monitor),
      loop(controlResult(Monitor, NewMonitor, Pid));
    {addValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, Pid} ->
      NewMonitor = 'Pollution':addValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, Monitor),
      loop(controlResult(Monitor, NewMonitor, Pid));
    {removeValue, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Pid} ->
      NewMonitor = 'Pollution':removeValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Monitor),
      loop(controlResult(Monitor, NewMonitor, Pid));
    {getOneValue, Date, Type, Station, Pid} ->
      loop(controlGetting(Monitor, 'Pollution':getOneValue(Date, Type, Station, Monitor), Pid));
    {getStationMean, Type, Station, Pid} ->
      loop(controlGetting(Monitor, 'Pollution':getStationMean(Type, Station, Monitor), Pid));
    {getDailyMean, Type, {Year, Month, Day}, Pid} ->
      loop(controlGetting(Monitor, 'Pollution':getDailyMean(Type, {Year, Month, Day}, Monitor), Pid));
    {exportToJson} ->
      io:format("~s~n", ['Pollution':exportToJSON(Monitor)]),
      loop(Monitor);
    stop ->
      io:format("Goodbye.~n"),
      ok
  end.

controlGetting(Monitor, {error, Description}, Pid) ->
  io:format("<Error> ~s~n", [Description]),
  Pid ! {error, Description},
  Monitor;
controlGetting(Monitor, V, Pid) ->
  io:format("~w~n", [V]),
  Pid ! Monitor,
  Monitor.

controlResult(Monitor, {error, Description}, Pid) ->
  io:format("<Error> ~s~n", [Description]),
  Pid ! {error, Description},
  Monitor;
controlResult(_, NewMonitor, Pid) ->
  io:format("~w~n", [NewMonitor]),
  Pid ! NewMonitor,
  NewMonitor.

receiveAnswer() ->
  receive
    _ -> ok
  end.



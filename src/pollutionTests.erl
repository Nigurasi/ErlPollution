%%%-------------------------------------------------------------------
%%% @author Anna
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. maj 2017 00:44
%%%-------------------------------------------------------------------
-module(pollutionTests).
-author("Anna").

-include_lib("eunit/include/eunit.hrl").

ifCreated_test() ->
  ?assertEqual('Pollution':createMonitor(), []).

ifAddedStationNotError_test() ->
  A = 'Pollution':createMonitor(),
  ?assertNotMatch({error, _}, 'Pollution':addStation("a", {2, 3}, A)).

ifAddedStation_test() ->
  A = 'Pollution':createMonitor(),
  ?assertEqual('Pollution':addStation("a", {2, 3}, A), [{station, "a", {2, 3}, []}]).

ifDailyMeanEmpty_test() ->
  ?assertEqual('Pollution':getDailyMean("temp", {2017, 5, 6}, 'Pollution':createMonitor()), 0).

ifDailyMeanExample_test() ->
  ?assert('Pollution':getDailyMean("temp", {2017, 5, 6}, createExample()) =:= 12.5).

createExample() ->
  A = 'Pollution':createMonitor(),
  B = 'Pollution':addStation("a", {2, 3}, A),
  C = 'Pollution':addStation("b", {2, 2}, B),
  D = 'Pollution':addValue("a", {{2017, 5, 6}, {15, 1, 2}}, "temp", 10, C),
  E = 'Pollution':addValue("b", {{2017, 5, 6}, {14, 1, 2}}, "temp", 15, D),
  E.

ifExportToJsonAlwaysAddBeginning_test() ->
  ?assertMatch("{'pollution':["++_, 'Pollution':exportToJSON(createExample())),
  ?assertMatch("{'pollution':["++_, 'Pollution':exportToJSON('Pollution':createMonitor())).

ifExportToJsonReturnsCorrectValue_test() ->
  ?assertEqual('Pollution':exportToJSON(createExample()),
    "{'pollution':[{'stationName':'b','coords':{'x':2,'y':2},'measurements':[{'date':{'year':2017,'month':5,'day':6,'hour':14,'minutes':1,'seconds':2},'type':'temp','value':15},]},{'stationName':'a','coords':{'x':2,'y':3},'measurements':[{'date':{'year':2017,'month':5,'day':6,'hour':15,'minutes':1,'seconds':2},'type':'temp','value':10},]},]}").

ifExportToJsonWithoutValues_test() ->
  A = 'Pollution':createMonitor(),
  B = 'Pollution':addStation("a", {2, 3}, A),
  C = 'Pollution':addStation("b", {2, 2}, B),
  ?assertEqual('Pollution':exportToJSON(C),
    "{'pollution':[{'stationName':'b','coords':{'x':2,'y':2},'measurements':[]},{'stationName':'a','coords':{'x':2,'y':3},'measurements':[]},]}").


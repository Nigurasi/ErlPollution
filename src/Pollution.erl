%%%-------------------------------------------------------------------
%%% @author Anna
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. kwi 2017 21:38
%%%-------------------------------------------------------------------
-module('Pollution').
-author("Anna").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, exportToJSON/1]).

% Structures
-record (measure, {date, type = "", value = 0}).
-record (station, {name = "Station", coords = {0, 0}, measurements = []}).

createMonitor() -> [].


%addStation/3 - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
addStation(StationName, Coordinates, Monitor) ->
  case findStation(StationName, Monitor)  of
    false -> case findStation(Coordinates, Monitor) of
               false -> [#station {name = StationName, coords = Coordinates} | Monitor];
                 _ -> {error, "An error occurred while adding a station. There is a station with same coordinates."}
             end;
    _ -> {error, "An error occurred while adding a station. There is a station with same name."}
  end.

findStation(_, []) -> false;
findStation(Station, [#station{name = Station} | _]) -> true;
findStation(Station, [#station{coords = Station} | _]) -> true;
findStation(Station, [_ | T]) -> findStation(Station, T).


%addValue/5 - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
addValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "An error occurred while adding a value. There is not a station with this detail."};
    _ -> wellAddValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, Monitor)
  end.

wellAddValue(_, _, _, _, []) -> [];
wellAddValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, [#station{name = Station, measurements = M} = S | T]) ->
  case getValue({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false ->
      [S#station{measurements = [ #measure {date = {{Year, Month, Day}, {Hour, Minutes, Seconds}}, type = Type, value = Value} | M]} | T];
      _ -> {error, "An error occurred while adding a value. There is a value with these measurements."}
  end;
wellAddValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, [#station{coords = Station, measurements = M} = S | T]) ->
  case getValue({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false ->
      [S#station{measurements = [ #measure {date = {{Year, Month, Day}, {Hour, Minutes, Seconds}}, type = Type, value = Value} | M]} | T];
    _ -> {error, "An error occurred while adding a value. There is a value with these measurements."}
  end;
wellAddValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, [H | T]) ->
  [H | wellAddValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, T)].


%removeValue/4 - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
removeValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "An error occurred while removing a value. There is not a station with this detail."};
    _ -> wellRemoveValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Monitor)
end.

wellRemoveValue(_, _, _, []) -> [];
wellRemoveValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, [#station{name = Station, measurements = M} = S | T]) ->
  case getValue({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false -> {error, "An error occurred while removing a value. There is no value with these measurements."};
    _ -> [justDoRemoveIt(S, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type) | T ]
  end;
wellRemoveValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, [#station{coords = Station, measurements = M} = S | T]) ->
  case getValue({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false -> {error, "An error occurred while removing a value. There is no value with these measurements."};
    _ -> [S#station{measurements = justDoRemoveIt(M, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type)} | T ]
  end;
wellRemoveValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, [H | T]) ->
  [H | wellRemoveValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, T)].

justDoRemoveIt([], _, _) -> [];
justDoRemoveIt([#measure{date = {{Year, Month, Day}, {Hour, Minutes, Seconds}}, type = Type} | T], {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type) ->
  T;
justDoRemoveIt([H | T], {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type) ->
 [H | justDoRemoveIt(T, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type)].


getValue(_, _, []) -> false;
getValue({{Year, Month, Day}, {Hour, _, _}}, Type,
    [#measure{date = {{Year, Month, Day}, {Hour, _, _}}, type = Type} | _]) -> true;
getValue({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, [_ | T]) ->
  getValue({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, T).


%getOneValue/4 - zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
getOneValue(_, _, _, []) -> {error, "An error occurred while getting a value. There is no station with this details."};
getOneValue(Date, Type, Station, [#station{ name = Station, measurements = M} | _]) -> getTheValue(Date, Type, M);
getOneValue(Date, Type, Station, [#station{ coords = Station, measurements = M} | _]) -> getTheValue(Date, Type, M);
getOneValue(Date, Type, Station, [_ | T]) -> getOneValue(Date, Type, Station, T).


getTheValue(_, _, []) -> {error, "An error occurred while getting a value. There is no value with this details."};
getTheValue(Date, Type, [#measure{date = Date, type = Type, value = V} | _]) -> V;
getTheValue(Date, Type, [_ | T]) -> getTheValue(Date, Type, T).


%getStationMean/3 - zwraca średnią wartość parametru danego typu z zadanej stacji;
getStationMean(_, _, []) -> {error, "An error occurred while getting a stationMean. There is no station with this details."};
getStationMean(Type, Station, [#station{name = Station, measurements = M} | _]) -> getMeasureMean(Type, M, 0, 0);
getStationMean(Type, Station, [#station{coords = Station, measurements = M} | _]) -> getMeasureMean(Type, M, 0, 0);
getStationMean(Type, Station, [_ | T]) -> getStationMean(Type, Station, T).

getMeasureMean(_, [], 0, 0) -> 0;
getMeasureMean(_, [], Length, Sum) -> Sum/Length;
getMeasureMean(Type, [#measure{type = Type, value = V} | T], Length, Sum) -> getMeasureMean(Type, T, Length + 1, Sum + V);
getMeasureMean(Type, [_ | T], Length, Sum) -> getMeasureMean(Type, T, Length, Sum).


%getDailyMean/3 - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;

getDailyMean(Type, {Year, Month, Day}, Monitor) ->
  getListValues(Type, {Year, Month, Day}, Monitor, {0, 0}).

getListValues(_, _, [], {0, 0}) -> 0;
getListValues(_, _, [], {Length, Sum}) -> Sum/Length;
getListValues(Type, {Year, Month, Day}, [#station{measurements = M} | T], {Length, Sum}) ->
  getListValues(Type, {Year, Month, Day}, T, getTypeValues(Type, {Year, Month, Day}, M, Length, Sum)).

getTypeValues(_, _, [], Length, Sum) -> {Length, Sum};
getTypeValues(Type, {Year, Month, Day}, [#measure{type = Type, date = {{Year, Month, Day}, {_, _, _}}, value = V} | T], Length, Sum) ->
  getTypeValues(Type, {Year, Month, Day}, T, Length + 1, Sum + V);
getTypeValues(Type, {Year, Month, Day}, [_ | T], Length, Sum) -> getTypeValues(Type, {Year, Month, Day}, T, Length, Sum).


%exportToJson - która przekształca dane przechowywane w monitorze do formatu JSON
exportToJSON(Monitor) -> exportStation (Monitor, "{\"pollution\":[").

exportStation([], Json) -> re:replace(Json ++ "]}", "\"", "'", [global, {return, list}]);
exportStation([#station{name = Name, coords = {X, Y}, measurements = M} | T], Json) ->
  exportStation(T, Json ++ "{\"stationName\":\"" ++ Name ++ "\",\"coords\":{\"x\":" ++ integer_to_list(X) ++ ",\"y\":"
    ++ integer_to_list(Y) ++ "},\"measurements\":[" ++ exportMeasurement(M, "") ++ "]},").

exportMeasurement([], Json) -> Json ++ "";
exportMeasurement([#measure{date = {{Year, Month, Day}, {Hour, Minutes, Seconds}}, type = Type, value = Value} | T], Json) ->
  exportMeasurement(T, Json ++ "{\"date\":{\"year\":" ++ integer_to_list(Year) ++ ",\"month\":" ++ integer_to_list(Month)
    ++ ",\"day\":" ++ integer_to_list(Day) ++ ",\"hour\":" ++ integer_to_list(Hour)
    ++ ",\"minutes\":" ++ integer_to_list(Minutes) ++ ",\"seconds\":" ++ integer_to_list(Seconds)
    ++ "},\"type\":\"" ++ Type ++ "\",\"value\":" ++ integer_to_list(Value) ++ "},").

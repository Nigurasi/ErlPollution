# ErlPollution
Module made on Erlang classes. Its purpuse is to contain and organize data on pollution.

The **structure** is simple: stations are organized in a list, each station contains information about its name, 
coordinates and measurements that are also organized in a list. Each measurement contains data on date, type of pollution and a value.

The module contains 8 **functions**:
- createMonitor/0 - to initalize a monitor which will contain all the data.
- addStation/3 - to create a station and add to the monitor.
- addValue/5 - to add a value to an exiting station in the monitor.
- removeValue/4 - to remove a value from an exiting station in the monitor.
- getOneValue/4 - to get an existing value based on station, type and data.
- getStationMean/3 - to get an average value of all values from given station.
- getDailyMean/3 - to get an average value of all values from given day.
- exportToJSON/1 - to export the monitor to JSON.



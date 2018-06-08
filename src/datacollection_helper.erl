%%%-------------------------------------------------------------------
%%% @author wangwd3
%%% @doc datacollection AI convert data to event 
%%% ------------------------------------------------------------------
-module(datacollection_helper).
-export([fetch/1,
         clear/1,
         delete/1,
         count/1,
         create/1,
         merge/2,
         save_data/4,
         save/1]).
-include("include/mongodbdriver.hrl").

save(Info) ->
    db_helper:save(Info#octopus_datacollection{time_updated = os:timestamp()}).

clear(Info) ->
  delete(Info).

delete(Id) ->
    Info = #octopus_datacollection{'_id'= Id},
    db_helper:delete(Info).

create(Info) ->
    Time = os:timestamp(),
    db_helper:create(Info#octopus_datacollection{time_created = Time, time_updated = Time}).

count(Info) when is_tuple(Info) ->
    db_helper:count(Info).

fetch(Id) when is_binary(Id)->
    Info = #octopus_datacollection{'_id' = Id},
    db_helper:fetch(Info);
fetch(Info) ->
    db_helper:fetch_list(Info).

merge(Old, New) ->
    db_helper:merge(Old, New).

save_data(UserId, Type, Data, Ts)->
    datacollection_helper:save(#octopus_datacollection{'_id' = uuid:uuid_bin(), 'user_id' = UserId, type = Type, data = Data, ts = Ts}).

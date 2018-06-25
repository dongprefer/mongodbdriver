%%%-------------------------------------------------------------------
%%% @author wangwd3
%%% @doc datacollection AI convert data to event 
%%% ------------------------------------------------------------------
-module(datacollection_helper).
-export([fetch/1,
         fetch/3,
         clear/1,
         delete/1,
         count/1,
         create/1,
         merge/2,
         save_data/4,
         save/1]).
-include("include/mongodbdriver.hrl").
-compile(export_all).

save(Info) ->
    Time = os:timestamp(),
    db_helper:save(Info#datacollection{time_updated = Time}).

clear(Info) ->
  delete(Info).

delete(Id) ->
    Info = #datacollection{'_id'= Id},
    db_helper:delete(Info).

create(Info) ->
    Time = os:timestamp(),
    db_helper:create(Info#datacollection{time_created = Time, time_updated = Time}).

count(Info) when is_tuple(Info) ->
    db_helper:count(Info).

fetch(Id) when is_binary(Id)->
    Info = #datacollection{'_id' = Id},
    db_helper:fetch(Info);
fetch(Info) ->
    db_helper:fetch_list(Info).
fetch(Info, Skip, Limit) ->
    db_helper:fetch_list(Info, Skip, Limit).

fetch_orderby_query() ->
    Record ={'$orderby', #datacollection{time_created = 1},'$query',   #datacollection{type= <<"3">> }},
    db_helper:fetch_list(Record).

merge(Old, New) ->
    db_helper:merge(Old, New).

save_data(Id, Type, Data, Ts)->
    datacollection_helper:save(#datacollection{'_id' = Id, type = Type, data = Data, ts = Ts}).

update_recordkey_setrecord(Id,Data) ->
    Time = os:timestamp(),
    Key = #datacollection{'_id' = Id},
    Record = {'$set', {
                        data, Data,
                        time_updated, Time
                      }},
    case fetch(Id) of
        {error, notfound} ->
            lager:debug("update_country_code but the Id = ~p is notfound", [Id]),
            [];
        {ok, UserInfo} ->           
            db_helper:update(Key, Record)
    end.

update_in(Ids, Data) ->
    Time = os:timestamp(),
    Key = #datacollection{'_id' = {'$in', Ids}},
    Record = {'$set', {
                         data,Data, 
                         time_updated, Time
                      }},
    db_helper:update(Key, Record).

fetch_all(Data) ->%=$and
    RuleInfo = {'$orderby', #datacollection{data = -1},
                '$query', #datacollection{data = {'$all', Data}}},
    db_helper:fetch_list(RuleInfo).

fetch_gt_lte(Datagt,Datalte) ->
    Record = {'$orderby', #datacollection{data = -1},
        '$query', #datacollection{
            data = {'$gt', Datagt, '$lte', Datalte}
        }
    },
    Res = db_helper:fetch_list(Record),
    case Res of
        {ok, Info} -> {ok, Info};
        {error, notfound} -> {ok, []}
    end.

%%and or
fetch_selfquery_and_in(Data1,UserId1,Data2,UserId2) ->
    ListTuple = [{ '$or',
                    [
                        {'$and',[{'data', Data1}, {'user_id', UserId1}]},
                       {'$and',[{'data', Data2}, {'user_id',UserId2 }]}
                    ]
                }],
    Tuple = to_tuple(ListTuple),
    Where = {'$orderby', #datacollection{time_created = -1}, '$self_query', Tuple},
    io:format("Where = ~p", [Where]),
    case db_helper:fetch_list(Where, 0, 100) of
        {error, notfound} ->
             ok;
        {ok, []} ->
             ok;
        {ok, Messages} ->
            io:format("Messages = ~p", [Messages])
   end.

to_tuple(List) ->
    ListNew = [{Key, Value} || {Key, Value} <- List , Value /= undefined],
    Fun = fun(Tuple, Acc) ->
             tuple_to_list(Tuple) ++ Acc
          end,
    Res = lists:foldl(Fun, [], ListNew),
    list_to_tuple(Res).


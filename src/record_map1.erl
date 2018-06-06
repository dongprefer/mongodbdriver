-module(record_map1).
-compile(export_all).
-record(datacollection, {
    '_id',
    user_id,
    type,
    ts,
    data}).

record_to_map()->
    Info = #datacollection{'_id'= <<"1234">>, data = <<"ok">>},
    %io:format("recordname = ~p,info=~p~n",[RecordName, Info]),
    Keys = record_info(fields, datacollection),
    [RecordName|Values] = tuple_to_list(Info),
    %io:format("keys = ~p,values=~p~n",[Keys,Values]),
    TupleLists = lists:zip(Keys, Values),
    maps:from_list(TupleLists).

map_to_record()->
    RecordName = datacollection,
    Map = #{'_id' => value_three,data => "value two"},
    List = maps:to_list(Map),
    Keys = record_info(fields, datacollection),
    Values = [
        case proplists:lookup(Key, List) of
          {_, Value} ->
            Value;
          none ->
            "";
          Any ->
             ""
        end
        || Key <- Keys
      ],
    Info = list_to_tuple([datacollection | Values]),
    Info#datacollection{}.

start_connection()->
    Type0 = sharded, 
    Host0 = ["10.128.130.241:27018","10.128.128.55:27018"], 
    Options0 = [{name,a},{pool_size,5},{pool_size,5},{ connectTimeoutMS, 20000 },{rp_mode,secondary}], 
    WorkerOptions0 = [{w_mode, master},{database, test}],
    {ok, Tsh} = mongo_api:connect(Type0, Host0, Options0, WorkerOptions0),
    Tsh.

insert(Info, Tsh)->
    io:format("Tsh =~p~n",[Tsh]),
    mongo_api:insert(Tsh, b, Info).
    
test()->
    Pid = start_connection(),
    Info = record_to_map(),
    insert(Info, Pid),
    ok.

    

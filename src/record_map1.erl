-module(record_map1).
-compile(export_all).
-include("include/mongodbdriver.hrl").
-record(datacollection, {
    '_id',
    user_id,
    type,
    ts,
    data}).

record_to_map()->
    Info = #datacollection{'_id'= <<"1243454">>, data = <<"ok">>},
    io:format("info=~p~n",[Info]),
    KeyOrg = record_info(fields, datacollection),
    Keys = [<<"_id">>] ++ KeyOrg -- ['_id'] ,
    io:format("keys = ~p~n",[Keys]),
    [RecordName|Values] = tuple_to_list(Info),
    io:format("keys = ~p,values=~p~n",[Keys,Values]),
    TupleLists = lists:zip(Keys, Values),
    maps:from_list(TupleLists).

record_to_map(Info, Keys)->
    io:format("keys = ~p~n",[Keys]),
    [RecordName|Values] = tuple_to_list(Info),
    io:format("keys = ~p,values=~p~n",[Keys,Values]),
    TupleLists = lists:zip(Keys, Values),
    maps:from_list(TupleLists).

map_to_record()->
    RecordName = datacollection,
    Map = #{'_id' => value_three,data => "value two"},
    List = maps:to_list(Map),
    KeyOrg = record_info(fields, datacollection),
    Keys = [<<"_id">>] ++ KeyOrg -- ['_id'] ,
    io:format("keys = ~p~n",[Keys]),
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
    Out = mongo_api:insert(Tsh, b, Info),
    io:format("out Info = ~p~n",[Out]),
    Out.

insert(Info, CollectionName, Tsh)->
    io:format("Tsh =~p~n",[Tsh]),
    Out = mongo_api:insert(Tsh, CollectionName, Info),
    io:format("out Info = ~p~n",[Out]),
    Out.
    
find_one(Selector, CollectionName, Tsh)->
    Projector = #{}, %#{<<"_id">> =>true}.
    mongo_api:find_one(Tsh, CollectionName, Selector, Projector).

update(Info, CollectionName, Tsh)->
    Pid = start_connection(),
    io:format("Tsh =~p~n",[Tsh]),
    Collection = datacollection,
    Selector = #{<<"_id">> => <<"123">>},
    Doc = #{<<"name">> => <<"dong">>},
    Opts = #{},
    Out = mongo_api:update(Pid, Collection, Selector, Doc, Opts),
    io:format("out Info = ~p~n",[Out]),
    Out.

test()->
    Pid = start_connection(),
    Info = record_to_map(),
    io:format("info =~p~n",[Info]),
    insert(Info, Pid),
    ok.

test_1()->
    Pid = db_connection:get_conn(),
    Info = #datacollection{'_id'= <<"1243454">>, data = <<"ok">>},
    io:format("info=~p~n",[Info]),
    KeyOrg = record_info(fields, datacollection),
    Keys = [<<"_id">>] ++ KeyOrg -- ['_id'] , 
    Info1 = record_to_map(Info,Keys),
    io:format("info =~p~n",[Info1]),
    insert(Info1, Pid),
    ok.

test_2()->
    Pid = db_connection:get_conn(),
    Info = #datacollection{'_id'= <<"1243454">>, data = <<"ok">>},
    io:format("info=~p~n",[Info]),
    KeyOrg = record_info(fields, datacollection),
    Keys = [<<"_id">>] ++ KeyOrg -- ['_id'] ,
    Info1 = record_to_map(Info,Keys),
    io:format("info =~p~n",[Info1]),
    find_one(Info1, datacollection, Pid),
    ok.


test_3()->
    Pid = db_connection:get_conn(),
    Info = #datacollection{'_id'= <<"1234567">>, data = <<"ok">>},
    io:format("info=~p~n",[Info]),
    KeyOrg = record_info(fields, datacollection),
    Keys = [<<"_id">>] ++ KeyOrg -- ['_id'] ,
    Info1 = record_to_map(Info,Keys),
    io:format("info =~p~n",[Info1]),
    update(Info1, datacollection, Pid),
    ok.

test_insert()->
    Id =  list_to_binary(integer_to_list(erlang:monotonic_time())), 
    Info = #octopus_datacollection{'_id' = Id, 'user_id' = <<"2222222222">>, type = <<"1">>, data = <<"3">>}, 
    datacollection_helper:create(Info).

test_find()->
    Info = #octopus_datacollection{'user_id' = <<"2222222222">>, type = <<"1">>, data = <<"3">>},
    datacollection_helper:fetch(Info).

test_count()->
    Info = #octopus_datacollection{'user_id' = <<"2222222222">>, type = <<"1">>, data = <<"3">>},
    datacollection_helper:count(Info).

test_delete()->
    %Info = #octopus_datacollection{'_id' = <<"-576460644743358709">>, type = <<"1">>, data = <<"3">>},
    Id = <<"-576460644743358709">>,
    datacollection_helper:clear(Id).

test_save()->
    Info = #octopus_datacollection{'_id' = <<"-576460275395216712">>,'user_id' = <<"2222222222">>, type = <<"1">>, data = <<"nihao">>},
    datacollection_helper:save(Info).

test_find_more(W)->
   [{record_map1:test_find(),A}||A <-lists:seq(1,W)].

test_insert_more(W)->
   [{record_map1:test_insert(),A}||A <-lists:seq(1,W)].

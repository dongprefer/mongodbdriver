-module(record_map).
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

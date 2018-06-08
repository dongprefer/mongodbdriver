-module(db_helper).
-export([fetch/1,
         fetch/2,
         create/1,
         delete/1,
         delete_all/1,
         fetch_list/1,
         fetch_list/2,
         fetch_list/3,
         merge/2,
         save/2,
         replace/2,
         update_ts/2,
         count/1,
         save/1,
         update/2,
         mogorel_save/1
         ]).

-include("include/mongodbdriver.hrl").


fetch_list(Record, Limit) ->
    fetch_list(Record, 0, Limit).

fetch_list(Record, Skip, Limit) ->
    DB = octopus,
    Fun = fun() ->
        Cursor = mongrel:find(Record, [], Skip, Limit),
        mongrel_cursor:take(Limit, Cursor)
        %mongrel_cursor:rest(Cursor)
    end,

    Res = mongo_helper:do_query("", DB, Fun),
    case Res of
        {ok, {}} -> {error, notfound};
        {ok, Info} -> {ok, Info}
    end.



fetch_list(Record) ->
    DB = octopus,
    Fun = fun() ->
        Cursor = mongrel:find(Record),
        mongrel_cursor:rest(Cursor)
    end,

    Res = mongo_helper:do_query("", DB, Fun),
    case Res of
        {ok, {}} -> {error, notfound};
        {ok, Infos} -> {ok, Infos}
    end.


fetch(Record, Projector) ->
    DB = octopus,
    Fun = fun() ->
        Cursor = mongrel:find(Record, Projector),
        mongrel_cursor:next(Cursor)
    end,

    Res = mongo_helper:do_query("", DB, Fun),
    case Res of
        {ok, {}} -> {error, notfound};
        {ok, Info} -> {ok, Info}
    end.

count(Record) ->
    DB = octopus,
    Fun = fun() ->
             mongrel:count(Record)
          end,
    case mongo_helper:do_query("", DB, Fun) of
        {ok, Num} ->
            Num;
        Error ->
            lager:error("Error = ~p", [Error]),
            0
    end.





fetch(Record) ->
    DB = octopus,
    case check_info(Record) of 
        true ->
            Fun = fun() ->
                Cursor = mongrel:find(Record),
                mongrel_cursor:next(Cursor)
            end,
            Res = mongo_helper:do_query("", DB, Fun),
            case Res of
                {ok, {}} -> {error, notfound};
                {ok, Info} -> {ok, Info}
            end;
        false ->
            {error, <<"lacking query condition">>}
    end.


save(Info) ->
    Module =get_backfun(Info),
    save(Info, Module).
    
save(Info, Module) ->
    Id = element(2, Info),
    lager:debug("Module = ~p", [Module]),
    lager:debug("Id = ~p", [Id]),
    DB = octopus,
    case Module:fetch(Id) of
        {ok, OldInfo} ->
            lager:debug("######1#####"),
            MergedInfo = Module:merge(OldInfo, Info),
            Fun = fun() -> 
                mongrel:save(MergedInfo) 
            end,
            {ok, ok} = mongo_helper:do_query("", DB, Fun),
            update_ts_inter(MergedInfo),
            {ok, MergedInfo};
        {error, notfound} ->
            Module:create(Info),
            {ok, Info};
        {error, Reason} ->
            lager:error("Reason = ~p", [Reason]),
            {error, Reason}
    end.

mogorel_save(Record) ->
   DB = octopus,
    Fun = fun() -> 
         mongrel:insert(Record) 
     end,
    {ok, ok} = mongo_helper:do_query("", DB, Fun).

update(Record, {'$set', Modifier})  ->
    update(Record, Modifier);
update(Record, Modifier)  ->
   DB = octopus,
    Fun = fun() -> 
         mongrel:modify(Record, {'$set', Modifier}) 
     end,
    {ok, ok} = mongo_helper:do_query("", DB, Fun).
 
update(Record)  ->
    mongrel:modify(Record).

replace(OldInfo, NewInfo) ->
    DB = octopus,
    Fun = fun() -> 
        mongrel:replace(OldInfo, NewInfo) 
    end,
    update_ts_inter(NewInfo),
    {ok, []} = mongo_helper:do_query("", DB, Fun),
    {OldInfo, NewInfo}.

merge(Old, Info) ->
    lager:debug("Old= ~p, Info= ~p", [Old, Info]),
    Len = tuple_size(Old),
    merge(Len, Old, Info).

merge(0, Old, _Info) ->
    Old;
merge(Len, Old, Info) ->
    lager:debug("Len=~p, Old=~p", [Len, element(Len, Info)]),
    New = 
        case element(Len, Info) of
            undefined -> Old;
            Value -> setelement(Len, Old, Value)
        end,
    merge(Len-1, New, Info).

create(Info) ->
    %record
    mongrel:insert(Info).

%% TODO the method can not delete all records 
delete_all(Info) ->
    DB = octopus,
    Fun = fun() ->
            update_ts_for_delete(Info),
            mongrel:delete(Info)
          end,
    {ok, ok} = mongo_helper:do_query("", DB, Fun),
    {ok, Info}.



delete(Info) ->
    DB = octopus,
    case check_info(Info) of 
        true ->
            update_ts_for_delete(Info),
            Fun = fun() ->
                    mongrel:delete(Info)
                  end,
            {ok, ok} = mongo_helper:do_query("", DB, Fun),
            {ok, Info};
        false ->
            {error, <<"lacking delete condition">>}
    end.


-spec get_backfun(Info::tuple()) -> atom().
get_backfun(Info) ->
    lager:debug("Info= ~p", [Info]),
    InfoName = 
        case list_to_binary(atom_to_list(element(1, Info))) of
            <<"octopus_log">> ->
                <<"octopus_log_helper">>;
            <<"octopus_", Name/binary>> ->
                <<Name/binary, "_helper">>;
            <<Name/binary>> ->
                <<Name/binary, "_helper">>
        end,
    List = binary_to_list(InfoName),
    lager:debug("List = ~p", [List]),
    catch case erlang:list_to_existing_atom(List) of
        {'EXIT', _Error} ->
            lager:debug("Error = ~p", [_Error]),
            list_to_atom(List);
        Module -> 
             Module
    end.

check(id, {ok, Id}) ->
  {ok, Id};
check(id, R) ->
    lager:error("id error Reason: ~p~n", [R]),
    {error, R};
check(mongo, {ok, Res}) ->
    {ok, Res};
check(mongo, {error, Reason}) ->
    lager:error("mongo error Reason: ~p~n", [Reason]),
    {error, Reason};
check(mongo, {failure, {_, ErrCode, _}} = Reason) ->
    lager:error("mongo error Reason: ~p, ErrCode = ~p~n", [Reason, ErrCode]),
    {error, Reason};
check(mongo, {failure, _R} = Reason) ->
    lager:error("mongo error Reason: ~p~n", [Reason]),
    {error, Reason};
check(mongo, R) ->
    lager:error("mongo error Reason: ~p~n", [R]),
    {error, R}.

check_info(Info) ->
    List = tuple_to_list(Info),
    FunLen = fun(undefined, Acc) -> 
                   Acc;
                (_, Acc) ->
                   Acc+1 
             end,
    Len = lists:foldl(FunLen, 0, List),
    Len >1. 

update_ts_inter(Info) ->
    Module =get_backfun(Info),
    case erlang:function_exported(Module, update_ts, 1) of
        true -> Module:update_ts(Info);
        false -> ok
    end.

-spec update_ts(UserId::binary(), TableName::binary()|atom()) -> {ok, tuple()}.
update_ts(UserId, TableName) when is_binary(TableName) ->
    ts_lib:update_ts(UserId, TableName);
update_ts(UserId, TableName) when is_atom(TableName) ->
    update_ts(UserId, list_to_binary(atom_to_list(TableName))).


update_ts_for_delete(Info) ->
    case fetch_list(Info) of
        {ok, List} ->
            [update_ts_inter(E) || E <-List];
        {error, notfound} ->
            ok
    end.

%%count_test() ->
%%    Exp = 9,
%%    Return = count(#octopus_hub_info{}),
%%    lager:debug("Return = ~p", [Return]),
%%    ok.
%%
%%update_test()  ->
%%   DB = octopus,
%%    Fun = fun() -> 
%%         mongrel:modify(#octopus_log{source_id = <<"a019b46a91bf8e64996baa3729a068f75018f4f1">>},
%%                        {'$set', #octopus_log{comet_id = <<"ssss">>}}) 
%%     end,
%%    {ok, ok} = mongo_helper:do_query("", DB, Fun).
%%
%%
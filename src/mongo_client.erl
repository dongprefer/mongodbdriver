-module(mongo_client).
-export([connect/1,
         close/1,
         map/0]).
-include_lib("mongrel/include/mongrel_macros.hrl").
-include("include/mongodbdriver.hrl").

connect(_DB) ->
    db_connection:get_conn().

map() ->
   mongrel_mapper:add_mapping(?mapping(datacollection)),
   mongrel_mapper:add_mapping(?mapping(user_info)),
   ok.

close(_MongoClientPools) ->
    ok.

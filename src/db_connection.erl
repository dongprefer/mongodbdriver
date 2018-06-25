%%% @wangwd 2018-06
-module(db_connection).
-behaviour(gen_server).

-export([start_link/0, 
         get_conn/0, 
         get_secondary_conn/0, 
         restart/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {topologymain, topologysecondary}).%topology is the pid that returns after connection.

start_link() ->
    mongo_client:map(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

restart() ->
    gen_server:call(?MODULE, {restart}).

get_conn() ->
    {ok,Connection} = gen_server:call(?MODULE, {get_conn}),
    Connection.

get_secondary_conn()->
    {ok,Connection} = gen_server:call(?MODULE, {get_secondary_conn}),
    Connection.

init([]) ->
    State = get_start(),
    process_flag(trap_exit, true),
    {ok, State}.

handle_call({restart}, _From, _State) ->
    NewState = get_start(),
    {reply, ok, NewState};
handle_call({get_conn}, _From, #state{topologymain = Topology} = State) ->
    {reply, {ok, Topology}, State};
handle_call({get_secondary_conn}, _From, #state{topologysecondary = Topology} = State) ->
    {reply, {ok, Topology}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({refresh}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    lager:notice("handler_info _Info = ~p", [_Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    lager:error("abormal termination: ~n   Scheduler id: ~p~n   Num scheduler:   ~p~n   Process count: ~p"
          "~n   Process limit:  ~p~n   Memory used by erlang processes:      ~p~n   Memory allocated by erlang processes: ~p"
          "~n   The total amount of memory allocated: ~p~n ",[SchedId, SchedNum, ProcCount, ProcLimit,ProcMemUsed, ProcMemAlloc, MemTot ]),
    lager:error("Reaseon = ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


get_start() ->
    TopologyMain = get_mainread_connection(),
    TopologySecondary = get_secondaryread_connection(),
    #state{topologymain = TopologyMain, topologysecondary = TopologySecondary}.

get_mainread_connection()->
    %{DbSet,BuzzMongoHosts} = us_config:get(<<"mongo">>, <<"oc">>),
    Type = sharded,
    Hosts = ["110.128.130.241:27018","110.128.128.55:27018"],
    Options = [{name,a},{pool_size,5},{ connectTimeoutMS, 20000 },{rp_mode,primaryPreferred}],
    WorkerOptions = [{w_mode, master},{database, test}],
    {ok, Topology} = mongo_api:connect(Type, Hosts, Options, WorkerOptions),
    Topology.

get_secondaryread_connection()->
    Type = sharded,
    Hosts = ["110.128.130.241:27018","110.128.128.55:27018"],
    Options = [{name,a},{pool_size,5},{ connectTimeoutMS, 20000 },{rp_mode,secondaryPreferred}],
    WorkerOptions = [{w_mode, master},{database, test}],
    {ok, Topology} = mongo_api:connect(Type, Hosts, Options, WorkerOptions),
    Topology.


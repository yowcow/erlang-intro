-module(my_bank).

-behavior(gen_server).

-export([
    start/0,
    stop/0,
    new_account/1,
    deposit/2,
    withdraw/2,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Client APIs
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, dict:new(), []).
stop()  -> gen_server:call(?MODULE, stop).

new_account(Who)      -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount)  -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).

% Server Callbacks
init(Dict) -> {ok, dict:store("backdoor", 1000, Dict)}.

handle_call({new, Who}, _From, Dict) ->
    {Reply, NewDict} = case dict:find(Who, Dict) of
        error -> {{welcome, Who}, dict:store(Who, 0, Dict)};
        _     -> {{Who, you_already_are_a_customer}, Dict}
    end,
    {reply, Reply, NewDict};
handle_call({add, _Who, Amount}, _From, Dict) when Amount < 0 ->
    {reply, you_can_only_add_more_than_0, Dict};
handle_call({add, Who, Amount}, _From, Dict) ->
    {Reply, NewDict} = case dict:find(Who, Dict) of
        error ->
            {not_a_customer, Dict};
        {ok, Balance} ->
            NewBalance = Balance + Amount,
            {{thanks, Who, your_balance_is, NewBalance}, dict:store(Who, NewBalance, Dict)}
    end,
    {reply, Reply, NewDict};
handle_call({remove, _Who, Amount}, _From, Dict) when Amount < 0 ->
    {reply, you_can_only_remove_more_than_0, Dict};
handle_call({remove, Who, Amount}, _From, Dict) ->
    {Reply, NewDict} = case dict:find(Who, Dict) of
        error ->
            {not_a_customer, Dict};
        {ok, Balance} when Balance >= Amount ->
            NewBalance = Balance - Amount,
            {{thanks, Who, your_balance_is, NewBalance}, dict:store(Who, NewBalance, Dict)};
        {ok, Balance} ->
            {{sorry, Who, you_only_have, Balance, in_the_bank}, Dict}
    end,
    {reply, Reply, NewDict};
handle_call(stop, _from, Dict) ->
    {stop, normal, stopped, Dict}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

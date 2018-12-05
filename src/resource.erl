-module(resource).

-export([start_link/0, stop/0, allocate/0, deallocate/0]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, allocate/2, deallocate/2, exited/2]).

-compile(export_all).

-behaviour(gen_server).

start() ->
    register(resource, spawn(resource, init, [])).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, na, []).

init(na) ->
    process_flag(trap_exit, true),
    {ok, {get_resources(),[]}}.

stop() ->
    gen_server:cast(?MODULE, stop).

get_resources() ->
    [1,2,3,4,5,6,7].

allocate() ->
    gen_server:call(?MODULE, {allocate, self()}).

deallocate() ->
    gen_server:call(?MODULE, {deallocate, self()}).

handle_cast(stop, ResourceList) ->
    {stop, normal, ResourceList}.

handle_call({allocate, Pid}, From, ResourceList) ->
    {NewResources, Reply} = allocate(ResourceList, Pid),
    {reply, Reply, NewResources};
handle_call({deallocate, Pid}, From, ResourceList) ->
    NewResources = deallocate(ResourceList, Pid),
    {reply, ok, NewResources}.

handle_info(Info, ResourceList) ->
    NewResources = exited(ResourceList, self()),
    {reply, exited, NewResources}.

allocate({[], Allocated}, Pid) ->
    {{[], Allocated}, {error, no_resources}};
allocate({[Resource|Resources], Allocated}, Pid) ->
    link(Pid),
    {{Resources, [{Resource, Pid} | Allocated]}, {ok,Resource}}.

deallocate({Free, Allocated}, Pid) ->
    {value, {Resource, Pid}} = lists:keysearch(Pid, 2, Allocated),
    unlink(Pid),
    NewAllocated = lists:keydelete({Resource, Pid}, 2, Allocated),
    {[Resource|Free], NewAllocated}.

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid, 2, Allocated) of
        {value,{Resource, Pid}} ->
            NewAllocated = lists:keydelete(Resource, 1, Allocated),
            {[Resource|Free], NewAllocated};
        false ->
            {Free, Allocated}
    end.


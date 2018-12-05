-module(clients).

-behaviour(gen_server).

-compile(export_all).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, noargs, []).

stop(Name) ->
    gen_server:cast(Name, stop).

allocate(Name) ->
    gen_server:call(Name, allocate).

deallocate(Name) ->
    gen_server:call(Name, deallocate).

init(noargs) ->
    % process_flag(trap_exit, true),
    {ok, undefined}.

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(allocate, _From, undefined) ->
    case resource:allocate() of
        {ok, Resource} ->
            {reply, {ok, Resource}, Resource};
        {error, no_frequencies} ->
            {reply, {error, no_frequencies}, undefined}
    end;
handle_call(deallocate, _From, Resource) ->
    resource:deallocate(),
    {reply, ok, undefined};
handle_call(allocate, _From, Resource) ->
    {reply, {already_allocated, Resource}, Resource}.

% handle_info(Info, Resource) ->
%     resource:deallocate(),
%     {stop, exited, Resource}.

-module(filterserver).

% Public API
-export([run/2, start/1]).

% Callback functions
-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1]).

-include("mailfilter.hrl").

-behaviour(gen_server).

-type filter_server() :: pid().

-type time() :: integer() | infinity.

-type callback() :: fun((filter_result()) -> none()).

-type context() :: {time(),
                    mail(),
                    filter(),
                    data(),
                    callback()}.

-record(state, {cap, running = #{}, queue = []}).

start(infinite) -> new(infinite);
start(Cap) when is_integer(Cap), Cap >= 1 -> new(Cap);
start(_) -> {error, invalid_capacity}.

new(Cap) -> gen_server:start(?MODULE, Cap, []).

run(FS, Context) -> run(FS, Context, infinity).

run(FS, {Mail, Filt, Data, Callback}, Time) ->
    gen_server:cast(FS,
                    {run, {Mail, Filt, Data, Callback, Time}}).

init(Cap) -> {ok, #state{cap = Cap}}.

num_running() -> ok.

handle_cast({run, Context},
            #state{cap = Cap, running = R, queue = Q} = S) ->
    case maps:size(R) of
        X when X < Cap ->
            {Pid, Ref} = execute(Context),
            {noreply, S#state{running = R#{Ref => Pid}}};
        X when X =:= Cap ->
            {noreply, S#state{queue = Q ++ [Context]}}
    end.

handle_call(_Type, _From, _S) -> ok.

handle_info({'DOWN', Ref, _, _, _},
            #state{running = R, queue = Q} = S) ->
    case maps:take(Ref, R) of
        {_, RS} ->
            case Q of
                [] -> {noreply, S#state{running = RS}};
                [X | XS] ->
                    {Pid, Ref} = execute(X),
                    {noreply, S#state{running = R#{Ref => Pid}}, queue = XS}
            end;
        error -> S
    end.

-spec execute(context()) -> {pid(), reference()}.

execute({Mail, Filt, Data, Callback, Time}) ->
    FS = self(),
    spawn_monitor(fun () ->
                          W = self(),
                          process_flag(trap_exit, true),
                          Pid = spawn_link(evaluator(FS,
                                                     W,
                                                     Filt,
                                                     Mail,
                                                     Data)),
                          receive
                              {'EXIT', Pid, _} -> Callback(unchanged);
                              Result -> Callback(Result)
                              after Time -> Callback(unchanged), exit(timeout)
                          end
                  end).

evaluator(FS, Watcher, Filter, Mail, Data) ->
    fun () -> Watcher ! evaluate(FS, Filter, Mail, Data)
    end.

callback() ->
    Me = self(),
    fun (Result) -> Me ! Result end.

-spec evaluate(filter_server(), filter(), mail(),
               data()) -> filter_result().

evaluate(FS, Filt, Mail, Data) ->
    case Filt of
        {simple, Fun} -> simple(Fun, Mail, Data);
        {chain, Filts} -> chain(FS, Filts, Mail, Data);
        {group, Filts, Merge} ->
            group(FS, Filts, Merge, Mail, Data);
        {timelimit, Time, Filt} ->
            timelimit(FS, Time, Filt, Mail, Data);
        _ -> unchanged
    end.

simple(Fun, Mail, Data) -> Fun(Mail, Data).

chain(_FS, [], _Mail, _Data) -> unchanged;
chain(FS, Filts, Mail, Data) ->
    {L, [Last]} = lists:split(length(Filts) - 1, Filts),
    {M, D} = lists:foldl(chainer(FS), {Mail, Data}, L),
    run(FS, {M, Last, D, callback()}),
    receive R -> R end.

chainer(FS) ->
    fun (Filt, {M, D}) ->
            run(FS, {M, Filt, D, callback()}),
            receive
                unchanged -> {M, D};
                {just, Data} -> {M, Data};
                {transformed, Mail} -> {Mail, D};
                {both, Mail, Data} -> {Mail, Data}
            end
    end.

group(FS, Filts, Merge, Mail, Data) ->
    Z = lists:zip(lists:seq(1, length(Filts)), Filts),
    lists:map(fun ({I, Filt}) ->
                      run(FS, {Mail, Filt, Data, index_callback(I)})
              end,
              Z),
    merge(Merge, [inprogress || _ <- Filts]).

index_callback(Index) ->
    Me = self(),
    fun (Res) -> Me ! {Index, Res} end.

merge(Merge, Results) ->
    receive
        {I, Result} ->
            R = insert(I, Result, Results),
            case Merge(R) of
                continue -> merge(Merge, R);
                M -> M
            end
    end.

insert(I, E, L) ->
    {L1, [_ | L2]} = lists:split(I - 1, L),
    L1 ++ [E] ++ L2.

timelimit(FS, Time, Filt, Mail, Data) ->
    run(FS, {Mail, Filt, Data, callback()}, Time).

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

-type callback() :: fun((filter_result()) -> none()).

-type context() :: {mail(),
                    filter(),
                    data(),
                    callback()}.

-record(state, {cap, running = #{}, queue = []}).

start(infinite) -> new(infinite);
start(Cap) when is_integer(Cap), Cap >= 1 -> new(Cap);
start(_) -> {error, invalid_capacity}.

new(Cap) -> gen_server:start(?MODULE, Cap, []).

run(FS, Context) -> gen_server:cast(FS, {run, Context}).

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

execute({Mail, Filter, Data, Callback}) ->
    Me = self(),
    spawn_monitor(fun () ->
                          R = evaluate(Me, Filter, Mail, Data),
                          Callback(R)
                  end).

-spec evaluate(filter_server(), filter(), mail(),
               data()) -> filter_result().

evaluate(FS, Filt, Mail, Data) ->
    case Filt of
        {simple, Fun} -> simple(Fun, Mail, Data);
        {chain, Filts} -> chain(FS, Filts, Mail, Data)
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

callback() ->
    Me = self(),
    fun (Res) -> Me ! Res end.

% evaluate(Filter, Mail, Data) ->
%     case Filter of

%         {group, [], _Merge} -> unchanged; % ASSUMPTION
%         {group, Filts, Merge} ->
%             Me = self(),
%             N = lists:seq(1, length(Filts)),
%             F = lists:zip(N, Filts),
%             lists:map(fun ({I, X}) ->

%                               spawn(fun () -> Me ! {I, eval(X, Mail, Data)} end)
%                       end,
%                       F),
%             g({[inprogress || _ <- N], Merge});
%         {timelimit, Time, Me} ->
%             Me = self(),
%             exit(self(), normal)
%     end.

% g({State, Merge}) ->
%     receive
%         {I, Result} ->
%             S = insert(I, Result, State),
%             case Merge(S) of
%                 continue -> g({S, Merge});
%                 R -> R
%             end
%     end.

% t(T) ->
%     Self = self(),
%     Pid = spawn(fun () -> Self ! {self(), ok} end),
%     receive
%         Result -> Result after T -> exit(Pid, normal), unchanged
%     end.

% insert(I, E, L) ->
%     {L1, [_ | L2]} = lists:split(I - 1, L),
%     L1 ++ [E] ++ L2.

% decide(Mail, Data, Res) ->
%     case Res of
%         unchanged -> {Mail, Data};
%         {transformed, M} -> {M, Data};
%         {just, D} -> {Mail, D};
%         {both, M, D} -> {M, D}
%     end.

% executer(Ref, C) -> fun() -> execute(Ref, C) end.

% execute(Ref, C) ->
%     R = evaluate(C, Filt),
%     gen_statem:cast(C#context.mr, {done, Ref, R}).

% evaluate(C, {simple, Fun}) -> Fun(C#context.mail, C#context.data);
% evaluate(C, {chain, []}) -> unchanged;
% evaluate(C, {chain, Filts}) ->
%     P = fun (F, {M, D, _}) ->
%         Me = self(),
%                          Func = fun () ->
%                             Me ! {result, evaluate(C#context{mail = M, data = D}, F)}
%                         end,
%                         mailserver:queue(C#context.ms, Func),
%                         R = receive Result -> Result end,
%                         {M2, D2} = decide(M, D, R),
%                         {M2, D2, R}
%                 end,
%             {_, _, Res} = lists:foldl(P, {Mail, Data, {}}, Filts),
%             Res.


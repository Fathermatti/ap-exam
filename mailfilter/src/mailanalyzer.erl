-module(mailanalyzer).

-behaviour(gen_statem).

% Public API
-export([add_filter/4,
         close/1,
         complete/1,
         evaluate/4,
         get_config/1,
         new/3]).

% Callback API
-export([active/3,
         callback_mode/0,
         inactive/3,
         init/1]).

-record(state, {ref, ms, mail, running, done}).

new(MS, Mail, Filters) ->
    gen_statem:start(?MODULE, {MS, Mail, Filters}, []).

add_filter(MR, Label, Filt, Data) ->
    gen_statem:cast(MR, {add, Label, Filt, Data}).

get_config(MR) -> gen_statem:call(MR, config).

close(MR) -> gen_statem:cast(MR, close).

complete(MR) -> gen_statem:call(MR, complete).

callback_mode() -> state_functions.

init({MS, Mail, Filters}) ->
    Ref = make_ref(),
    run(MS, self(), Ref, Mail, Filters),
    {ok,
     active,
     #state{ref = Ref, ms = MS, mail = Mail,
            running = Filters, done = #{}}}.

active(cast, {done, Ref, Label, {just, Data}},
       #state{ref = Ref} = S) ->
    {keep_state, commit(S, Label, Data)};
active(cast, {done, Ref, Label, unchanged},
       S = #state{ref = Ref}) ->
    {keep_state, unchanged(S, Label)};
active({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]};
active({call, From}, complete, S) ->
    {next_state, inactive, S, [{reply, From, result(S)}]};
active(cast, close, S) -> {next_state, inactive, S}.

% active(cast, {add, Label, Filt, Data},
%           #state{ms = MS, running = Running, done = Done} = S) ->
%     case lists:any(fun (X) -> X =:= Label end, Running) of
%         true -> {keep_state, S};
%         false ->
%             mailserver:execute(MS,
%                                fun () -> io:fwrite("Hello world 2~n", []) end),
%             {keep_state,
%              S#state{running = [{Label, Filt, Data} | Running]}}
%     end;

inactive(cast, {add, _, _, _}, _S) ->
    {keep_state_and_data};
inactive(cast, close, _S) -> {keep_state_and_data};
inactive({call, From}, complete, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]};
inactive({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]}.

result(#state{mail = Mail, running = Running,
              done = Done}) ->
    {Mail,
     maps:values(maps:map(fun (Label, {_, _}) ->
                                  {Label, inprogress}
                          end,
                          Running))
         ++
         maps:values(maps:map(fun (Label, {_, Data}) ->
                                      {Label, {done, Data}}
                              end,
                              Done))}.

run(MS, MR, Ref, Mail, Filters) ->
    maps:map(fun (Label, {Filter, Data}) ->
                     mailserver:execute(MS,
                                        executer(MS,
                                                 MR,
                                                 Ref,
                                                 Mail,
                                                 Label,
                                                 Filter,
                                                 Data))
             end,
             Filters).

commit(#state{running = R, done = D} = S, Label,
       Data) ->
    case maps:take(Label, R) of
        {Running, Rest} ->
            S#state{running = Rest,
                    done = D#{Label => {Running, Data}}};
        error -> S
    end.

unchanged(#state{running = R, done = D} = S, Label) ->
    case maps:take(Label, R) of
        {{Filter, Data}, Rest} ->
            S#state{running = Rest,
                    done = D#{Label => {{Filter, Data}, Data}}};
        error -> S
    end.

transform(#state{running = R, done = D} = S, Label,
          Mail) ->
    case maps:take(Label, R) of
        {{Filter, Data}, Rest} ->
            Re = maps:map(fun (Label, {{Filter, Data}, _}) ->
                                  {Filter, Data}
                          end,
                          D),
            S#state{mail = Mail, running = Re,
                    done = #{Label => {{Filter, Data}, Data}}};
        error -> S
    end.

executer(MS, MR, Ref, Mail, Label, Filter, Data) ->
    fun () ->
            R = evaluate(MS, Filter, Mail, Data),
            gen_statem:cast(MR, {done, Ref, Label, R})
    end.

evaluation_executer(MS, Filt, Mail, Data) ->
    Me = self(),
    fun () -> Me ! evaluate(MS, Filt, Mail, Data) end.

evaluate(MS, {simple, Fun}, Mail, Data) ->
    Fun(Mail, Data);
evaluate(MS, {chain, []}, Mail, Data) -> unchanged;
evaluate(MS, {chain, Filts}, Mail, Data) ->
    {L, [Last]} = lists:split(length(Filts) - 1, Filts),
    P = fun (F, {M, D}) ->
                mailserver:execute(MS,
                                   evaluation_executer(MS, F, M, D)),
                receive
                    unchanged -> {M, D};
                    {just, Data} -> {M, Data};
                    {transformed, Mail} -> {Mail, D};
                    {both, Mail, Data} -> {Mail, Data}
                end
        end,
    {M, D} = lists:foldl(P, {Mail, Data}, L),
    mailserver:execute(MS,
                       evaluation_executer(MS, Last, M, D)),
    receive R -> R end.

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


-module(analyzer).

-behaviour(gen_statem).

% Public API
-export([new/2]).

% Callback API
-export([analyzing/3, callback_mode/0, done/3, init/1]).

-export([eval/3]).

-record(mail, {mail, inprogress, done}).

new(Mail, Filts) ->
    case gen_statem:start(?MODULE, {Mail, Filts}, []) of
        ignore -> {error, ignore};
        {error, Err} -> {error, Err};
        {ok, Pid} ->
            gen_statem:cast(Pid, start),
            {ok, Pid}
    end.

callback_mode() -> state_functions.

init({Mail, Filts}) ->
    {ok,
     analyzing,
     #mail{mail = Mail, inprogress = Filts, done = #{}}}.

analyzing({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]};
analyzing(cast, start, S) ->
    {keep_state, S, [{next_event, internal, execute}]};
analyzing(internal, execute, S) ->
    New = evaluate(S),
    case maps:size(New#mail.inprogress) of
        0 -> {next_state, done, New};
        _ ->
            {keep_state, New, [{next_event, internal, execute}]}
    end.

done({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]}.

result(#mail{inprogress = I, done = D}) ->
    II = maps:map(fun (_K, _) -> inprogress end, I),
    DD = maps:map(fun (_K, {_, Data}) -> {done, Data} end,
                  D),
    maps:to_list(II) ++ maps:to_list(DD).

next(Map) ->
    I = maps:iterator(Map),
    {Label, _, _} = maps:next(I),
    Label.

evaluate(S = #mail{mail = Mail, inprogress = IP,
                   done = Done}) ->
    Label = next(IP),
    case maps:take(Label, IP) of
        {{Filt, Data}, New} ->
            Res = eval(Filt, Mail, Data),
            {_M, D} = decide(Mail, Data, Res),
            S#mail{inprogress = New,
                   done = maps:put(Label, {{Filt, Data}, D}, Done)};
        error -> S
    end.

eval(Filt, Mail, Data) ->
    case Filt of
        {simple, Fun} -> Fun(Mail, Data);
        {chain, []} -> unchanged;
        {chain, Filts} ->
            P = fun (F, {M, D, _}) ->
                        R = eval(F, M, D),
                        {M2, D2} = decide(M, D, R),
                        {M2, D2, R}
                end,
            {_, _, Res} = lists:foldl(P, {Mail, Data, {}}, Filts),
            Res;
        {group, [], _Merge} -> unchanged; % ASSUMPTION
        {group, Filts, Merge} ->
            Me = self(),
            N = lists:seq(1, length(Filts)),
            F = lists:zip(N, Filts),
            lists:map(fun ({I, X}) ->
                              spawn(fun () -> Me ! {I, eval(X, Mail, Data)} end)
                      end,
                      F),
            g({[inprogress || _ <- N], Merge});
        {timelimit, Time, Me} ->
            Me = self(),
            exit(self(), normal

    end.

g({State, Merge}) ->
    receive
        {I, Result} ->
            S = insert(I, Result, State),
            case Merge(S) of
                continue -> g({S, Merge});
                R -> R
            end
    end.

t(T) ->
  Self = self(),
  Pid = spawn(fun()-> 
                  timer:sleep(2000),
                  io:format("hello world ~p!~n",[Name]),
                  Self ! {self(), ok} end),
  receive
    Result -> Result
  after
     T -> exit(Pid, normal),
     unchanged
  end.

insert(I, E, L) ->
    {L1, [_ | L2]} = lists:split(I - 1, L),
    L1 ++ [E] ++ L2.

decide(Mail, Data, Res) ->
    case Res of
        unchanged -> {Mail, Data};
        {transformed, M} -> {M, Data};
        {just, D} -> {Mail, D};
        {both, M, D} -> {M, D}
    end.

-module(test_mailfilter_qc).

-export([wellbehaved_filter_fun/0]). % Remember to export the other function from Q2.2

-include_lib("eqc/include/eqc.hrl").

-include("apqc_statem.hrl").

-include("mailfilter.hrl").

-behaviour(apqc_statem).

-export([command/1,
         initial_state/0,
         next_state/3,
         postcondition/3,
         precondition/2]).

-compile(export_all).

%test_all() -> eunit:test(register_test_(), [verbose]).

results() ->
    eqc_gen:elements([{unchanged,
                       {just, truth},
                       {transformed, newmail},
                       {both, data, mail}}]).

transformed_result() ->
    eqc_gen:elements([{transformed, newmail},
                      {both, data, mail}]).

-type generator() :: fun(() -> filter_fun()).

%qTHAT GETCONFIG AND STOP RETURNS THE SAME

-spec wellbehaved_filter_fun() -> filter_fun().

g(Time) ->
    oneof([fun (_M, _D) ->
                   timer:sleep(Time rem 1000),
                   unchanged
           end,
           fun (M, D) ->
                   timer:sleep(Time rem 1000),
                   {just, {M, D}}
           end,
           fun (M, D) ->
                   timer:sleep(Time rem 1000),
                   {transformed, {M, D}}
           end,
           fun (M, D) ->
                   timer:sleep(Time rem 1000),
                   {both, {M, D}, {M, D}}
           end]).

merge() ->
    fun (Results) ->
            case lists:any(fun (X) -> X =:= inprogress end, Results)
                of
                true -> continue;
                false -> {just, hey}
            end
    end.

filter(FunGen) -> ?SIZED(Size, (filter(Size, FunGen))).

filter(0, FunGen) -> ?LET(Filt, FunGen, {simple, Filt});
filter(Size, FunGen) ->
    ?LAZY((oneof([?LET(Filt, FunGen, {simple, Filt}),
                  ?LET(Filts, (list(filter(Size div 2, FunGen))),
                       {chain, Filts}),
                  ?LET(Filts, (list(filter(Size div 2, FunGen))),
                       {group, Filts, merge()}),
                  ?LET({Time, Filts}, {int(), filter(Size div 2, FunGen)},
                       {timelimit, abs(Time * 100) rem 1000, Filts})]))).

wellbehaved_filter_fun() ->
    ?LET(R, (g(int())), fun (_M, _D) -> R end).

transforming_filter_fun() ->
    ?LET(R, (transformed_result()), fun (_M, _D) -> R end).

mail() -> eqc_gen:utf8().

-type model_state() :: #{ms := pid(), mails := [any()]}.

-spec initial_state() -> model_state().

initial_state() -> #{ms => none, mails => []}.

start(Cap) ->
    {ok, MS} = mailfilter:start(Cap),
    MS.

add_mail(MS, Mail) ->
    {ok, MR} = mailfilter:add_mail(MS, Mail),
    MR.

command(#{ms := none}) ->
    return({call, ?MODULE, start, [infinite]});
command(#{ms := MS, mails := []}) ->
    return({call, ?MODULE, add_mail, [MS, mail()]});
command(#{ms := MS, mails := Mails}) ->
    [{MR, _Mail} | _] = Mails,
    oneof([{call, ?MODULE, add_mail, [MS, mail()]},
           {call, mailfilter, enough, [MR]}]).

next_state(S, V, {call, ?MODULE, start, [_Cap]}) ->
    S#{ms := V};
next_state(#{mails := Mails} = S, MR,
           {call, ?MODULE, add_mail, [_MS, Mail]}) ->
    S#{mails := [ {MR, Mail} | Mails]};
next_state(#{mails := Mails} = S, _V,
           {call, mailfilter, enough, [MR]}) ->
    S#{mails := proplists:delete(MR, Mails)};
next_state(S, _V, _C) ->
    S.

precondition(_S, _) -> true.

postcondition(_S, _, _R) -> true.

prop_mail_is_sacred() ->
    ?FORALL(Cmds, (commands(?MODULE)),
            begin
                {_H, #{ms := MS, mails := Mails}, _R} = Result =
                                                            run_commands(?MODULE,
                                                                         Cmds),
                                                                         timer:sleep(10),
                State = stop(MS),
                check_commands(Cmds, Result, Mails, State),
                same_mails(Mails, State)
            end).

stop(none) -> [];
stop(MS) ->
    {ok, State} = mailfilter:stop(MS),
    State.

same_mails(Mails, State) ->
    lists:sort([Mail || {_MR, Mail} <- Mails]) =:=
        lists:sort([Mail || {Mail, _} <- State]).

check_commands(Cmds, {_, _, Res} = HSRes, Mails, State) ->
    pretty_commands(?MODULE,
                    Cmds,
                    HSRes,
                    aggregate(command_names(Cmds), same_mails(Mails, State) )).

instant_filter_fun() ->
    ?LET(A, (eqc_gen:int()),
         fun (M, _D) ->
                 case M of
                     [] -> {transformed, [A]};
                     [X] -> {just, [A | X]}
                 end
         end).

prop_insert_post() ->
    ?FORALL({F1, F2},
            {filter(instant_filter_fun()),
             filter(instant_filter_fun())},
            begin
                {ok, MS} = mailfilter:start(infinite),
                mailfilter:default(MS, x, F1, []),
                mailfilter:default(MS, y, F2, []),
                mailfilter:add_mail(MS, []),
                {ok, S} = mailfilter:stop(MS),
                ?IMPLIES((done(S)), (finished(S)))
            end).

any_inprogress(Config) ->
    lists:any(fun ({_L, Result}) -> Result =:= inprogress
              end,
              Config).

g() ->
    {utf8(), filter(transforming_filter_fun()), int()}.

prop_always_transforming() ->
    ?FORALL({X, Y, XS}, {g(), g(), eqc_gen:list(5, g())},
            begin
                {ok, MS} = mailfilter:start(infinite),
                lists:map(fun ({L, F, D}) ->
                                  mailfilter:default(MS, L, F, D)
                          end,
                          [X] ++ [Y] ++ XS),
                {ok, MR} = mailfilter:add_mail(MS, mail),
                {ok, C} = mailfilter:get_config(MR),
                any_inprogress(C)
            end).

done([{_M, [{_, {done, _}}, {_, {done, _}}]}]) -> true;
done(_) -> false.

finished([{[M],
           [{x, {done, [M]}}, {y, {done, [_ | M]}}]}]) ->
    true;
finished([{[M],
           [{x, {done, [_ | M]}}, {y, {done, [M]}}]}]) ->
    true;
finished(_) -> false.

run() -> eqc:quickcheck(prop_mail_is_sacred()).

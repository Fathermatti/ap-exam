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

-type generator() :: fun(() -> filter_fun()).

%qTHAT GETCONFIG AND STOP RETURNS THE SAME
% Given to always transforming mails, one is always pending

-spec wellbehaved_filter_fun() -> filter_fun().

wellbehaved_filter_fun() ->
    ?LET(R, (results()), fun (_M, _D) -> R end).

filter_kind() ->
    eqc_gen:elements([{unchanged,
                       {just, truth},
                       {transformed, newmail},
                       {both, data, mail}}]).

-spec filter(generator()) -> filter().

filter(FunGen) ->
    ?LET(R, FunGen, (oneof([{simple, R}]))).

mail() -> eqc_gen:utf8().

-type model_state() :: #{ms := pid(), mails := [any()]}.

-spec initial_state() -> model_state().

initial_state() -> #{ms => none, mails => []}.

start(Cap) ->
    {ok, MS} = mailfilter:start(Cap),
    MS.

command(#{ms := none}) ->
    return({call, ?MODULE, start, [infinite]});
command(#{ms := MS}) ->
    oneof([{call, mailfilter, add_mail, [MS, mail()]}]).

next_state(S, V, {call, ?MODULE, start, [_Cap]}) ->
    S#{ms := V};
next_state(#{mails := Mails} = S, _V,
           {call, mailfilter, add_mail, [_MS, Mail]}) ->
    S#{mails := [Mail | Mails]}.

precondition(_S, {call, _, _, _}) -> true.

postcondition(_S, {call, _, _, _}, _R) -> true.

prop_mail_is_sacred() ->
    ?FORALL(Cmds, (commands(?MODULE)),
            begin
                {H, S, R} = Result = run_commands(?MODULE, Cmds),
                #{ms := MS, mails := Mails} = S,
                Labelled = case MS of
                               none -> [];
                               MS ->
                                   {ok, L} = mailfilter:stop(MS),
                                   L
                           end,
                pretty_commands(?MODULE,
                                Cmds,
                                Result,
                                aggregate(command_names(Cmds),
                                          R =:= ok andalso
                                              same_mails(Mails, Labelled)))
            end).

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

done([{_M, [{_, {done, _}}, {_, {done, _}}]}]) -> true;
done(_) -> false.

finished([{[M],
           [{x, {done, [M]}}, {y, {done, [_ | M]}}]}]) ->
    true;
finished([{[M],
           [{x, {done, [_ | M]}}, {y, {done, [M]}}]}]) ->
    true;
finished(_) -> false.

same_mails(List, Labelled) ->
    lists:sort(List) =:=
        lists:sort([Mail || {Mail, _} <- Labelled]).

run() -> eqc:quickcheck(prop_insert_post()).

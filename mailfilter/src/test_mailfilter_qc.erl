-module(test_mailfilter_qc).

-export([wellbehaved_filter_fun/0]). % Remember to export the other function from Q2.2

% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_mailfilter.
-include_lib("eqc/include/eqc.hrl").

-include("apqc_statem.hrl").

-behaviour(apqc_statem).

-export([command/1,
         initial_state/0,
         next_state/3,
         postcondition/3,
         precondition/2]).

-compile(export_all).

%test_all() -> eunit:test(register_test_(), [verbose]).

-type mail() :: any().

-type data() :: any().

-type label() :: any().

-type result() :: {done, data()} | inprogress.

-type labelled_result() :: {label(), result()}.

-type filter_result() :: {just, data()} |
                         {transformed, mail()} |
                         unchanged |
                         {both, mail(), data()}.

-type filter_fun() :: fun((mail(),
                           data()) -> filter_result()).

-type filter() :: {simple, filter_fun()} |
                  {chain, [filter()]} |
                  {group, [filter()], merge_fun()} |
                  {timelimit, timeout(), filter()}.

-type merge_fun() :: fun(([filter_result() |
                           inprogress]) -> filter_result() | continue).

results() ->
    eqc_gen:elements([{unchanged,
                       {just, truth},
                       {transformed, newmail},
                       {both, data, mail}}]).

-type generator() :: fun(() -> filter_fun()).

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
        {ok, MS } = mailfilter:start(Cap),
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

prop_registration() ->
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

same_mails(List, Labelled) ->
    lists:sort(List) =:=
        lists:sort([Mail || {Mail, _} <- Labelled]).

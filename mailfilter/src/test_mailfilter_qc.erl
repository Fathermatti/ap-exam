-module(test_mailfilter_qc).

-export([wellbehaved_filter_fun/0]). % Remember to export the other function from Q2.2

% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_mailfilter.
-include_lib("eqc/include/eqc.hrl").
-compile(export_all).
%test_all() -> eunit:test(register_test_(), [verbose]).

-type mail() :: any().
-type data() :: any().
-type label() :: any().
-type result() :: {done, data()} | inprogress . -type labelled_result() :: {label(), result()}. -type filter_result() :: {just, data()}
| {transformed, mail()}
| unchanged
| {both, mail(), data()}.
-type filter_fun() :: fun( (mail(), data()) -> filter_result() ).

-type filter() :: {simple, filter_fun()} | {chain, list(filter())}
| {group, list(filter()), merge_fun() } | {timelimit, timeout(), filter()}.
-type merge_fun() :: fun( (list(filter_result() | inprogress)) -> filter_result() | continue ).

results() -> eqc_gen:elements([{unchanged, {just, truth}, {transformed, newmail}, {both, data, mail}}]).

-type generator() :: fun(() -> filter_fun() ).

-spec wellbehaved_filter_fun() -> filter_fun().
wellbehaved_filter_fun() -> 
    ?LET(R, results(),
         fun(_M, _D) -> R end).

filter_kind() -> eqc_gen:elements([{unchanged, {just, truth}, {transformed, newmail}, {both, data, mail}}]).

-spec filter(generator()) -> filter().
filter(FunGen) -> 
    ?LET(R, FunGen,
    oneof([{simple, R}])).

prop_mail_is_sacred() ->
    ?FORALL(T, bst(atom_key(), int_value()),
            valid(T)).

mail() -> eqc_gen:utf8().

-type model_state() ::
#{ mails := [any()] }.

-spec initial_state() -> model_state().
initial_state() -> #{mails => []}.

command(S) ->
       oneof(
         [{call,mailfilter, add_mail, [S, mail()]}, 
        {call,mailfilter,unregister,[name()]}, 
        {call,mailfilter,spawn,[]}]).


prop_erase()->
        ?FORALL(D, dict(),
        ?FORALL(K, key_from(D),
        beginDict=eval(D),
        equals(model(dict:erase(K,Dict)),
        model_erase(K,model(Dict)))end)).



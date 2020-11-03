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

transformed_result() ->
    eqc_gen:elements([{transformed, newmail},
                      {both, data, mail}]).

filter_fun(Time) ->
    ?LET(T, Time,
         begin
             oneof([fun (_M, _D) ->
                            timer:sleep(T rem 1000),
                            unchanged
                    end,
                    fun (M, D) ->
                            timer:sleep(T rem 1000),
                            {just, {M, D}}
                    end,
                    fun (M, D) ->
                            timer:sleep(T rem 1000),
                            {transformed, {M, D}}
                    end,
                    fun (M, D) ->
                            timer:sleep(T rem 1000),
                            {both, {M, D}, {M, D}}
                    end])
         end).

wellbehaved_filter_fun() -> filter_fun(int()).

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

merge() ->
    fun (Results) ->
            case lists:any(fun (X) -> X =:= inprogress end, Results)
                of
                true -> continue;
                false -> {just, hey}
            end
    end.

prop_mail_is_sacred() ->
    ?FORALL(Cmds, (commands(?MODULE)),
            begin
                {_H, #{ms := MS, mails := Mails}, _R} =
                    run_commands(?MODULE, Cmds),
                timer:sleep(10),
                same_mails(Mails, stop(MS))
            end).


filter_fun() ->oneof([fun (M, D) ->
                            {just, M}
                    end,
                    fun (M, D) ->
                            {transformed, M}
                    end,
                    fun (M, D) ->
                            {both, M, M}
                    end]).

prop_consistency() ->
    ?FORALL({T1, T2},
            {transforming(), transforming()},
            begin 
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, x, T1, none),
        mailfilter:default(MS, y, T2, none),
        {ok, _} = mailfilter:add_mail(MS, Mail),
        timer:sleep(100),
        {ok, State} = mailfilter:stop(MS),
        T1()
        end
    ).

%qTHAT GETCONFIG AND STOP RETURNS THE SAME

stop(none) -> [];
stop(MS) ->
    {ok, State} = mailfilter:stop(MS),
    State.

mail() -> eqc_gen:utf8().

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
    S#{mails := [{MR, Mail} | Mails]};
next_state(#{mails := Mails} = S, _V,
           {call, mailfilter, enough, [MR]}) ->
    S#{mails := proplists:delete(MR, Mails)};
next_state(S, _V, _C) -> S.

precondition(_S, _) -> true.

postcondition(_S, _, _R) -> true.

same_mails(Mails, State) ->
    lists:sort([Mail || {_MR, Mail} <- Mails]) =:=
        lists:sort([Mail || {Mail, _} <- State]).

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

any_inprogress(Config) ->
    lists:any(fun ({_L, Result}) -> Result =:= inprogress
              end,
              Config).

-module(test_mailfilter_eunit).

-export([]). % Remember to export the other function from Q2.2

-include_lib("eunit/include/eunit.hrl").

%test_all() -> eunit:test(register_test_(), [verbose]).

start_test_() ->
    [{"Start inifinite capacity",
      fun () ->
              ?assertMatch({ok, _}, (mailfilter:start(infinite)))
      end},
     {"Start valid capacity",
      fun () -> ?assertMatch({ok, _}, (mailfilter:start(100)))
      end},
     {"Start zero capacity",
      fun () ->
              ?assertMatch({error, invalid_capacity},
                           (mailfilter:start(0)))
      end},
     {"Start invalid capacity",
      fun () ->
              ?assertMatch({error, invalid_capacity},
                           (mailfilter:start(-100)))
      end},
     {"Start noninteger capacity",
      fun () ->
              ?assertMatch({error, invalid_capacity},
                           (mailfilter:start(#{x => 2})))
      end}].

stop_test_() ->
    [{"Stop running server",
      fun () ->
              {ok, MS} = mailfilter:start(infinite),
              {ok, _} = mailfilter:add_mail(MS, "x"),
              ?assertMatch({ok, [{"x", []}]}, (mailfilter:stop(MS)))
      end},
     {"Stop nonrunning server",
      fun () ->
              ?assertMatch({error, _}, (mailfilter:stop(pid)))
      end}].

stop_state_test_() ->
    [{"State with single mail, default filter",
      {setup,
       fun start/0,
       fun (MS) ->
               mailfilter:default(MS, x, simple(), none),
               {ok, _} = mailfilter:add_mail(MS, "x"),
               {ok, State} = mailfilter:stop(MS),
               ?_assertMatch([{"x", [{x, _}]}], State)
       end}},
     {"State with single mail, default and "
      "added filters",
      {setup,
       fun start/0,
       fun (MS) ->
               mailfilter:default(MS, x, simple(), none),
               {ok, MR} = mailfilter:add_mail(MS, "x"),
               mailfilter:add_filter(MR, y, simple(), none),
               {ok, State} = mailfilter:stop(MS),
               [{_Mail, Config}] = State,
               Labels = [L || {L, _} <- Config],
               [?_assert((lists:member(x, Labels))),
                ?_assert((lists:member(y, Labels))),
                ?_assertMatch([{"x", _Config}], State),
                ?_assertMatch([{"x", _Config}], State)]
       end}}].

default_test_() ->
    [{"Add default filter",
      {setup,
       fun start/0,
       fun (MS) ->
               ?_assertMatch(ok,
                             (mailfilter:default(MS, x, simple(), 0)))
       end}},
     {"Multiple defaults are added to mail",
      {setup,
       fun start/0,
       fun (MS) ->
               mailfilter:default(MS, x, simple(), 0),
               mailfilter:default(MS, y, simple(), 0),
               {ok, MR} = mailfilter:add_mail(MS, "x"),
               {ok, Config} = mailfilter:get_config(MR),
               Labels = [L || {L, _} <- Config],
               [?_assert((lists:member(x, Labels))),
                ?_assert((lists:member(y, Labels)))]
       end}},
     {"Duplicate default registers once",
      {setup,
       fun start/0,
       fun (MS) ->
               mailfilter:default(MS, x, simple(), 0),
               mailfilter:default(MS, x, simple(), 0),
               {ok, MR} = mailfilter:add_mail(MS, "x"),
               {ok, Config} = mailfilter:get_config(MR),
               ?_assertMatch([{x, _}], Config)
       end}},
     {"Default added after mail registration "
      "not added",
      {setup,
       fun start/0,
       fun (MS) ->
               mailfilter:default(MS, x, simple(), 0),
               {ok, MR} = mailfilter:add_mail(MS, "x"),
               mailfilter:default(MS, y, simple(), 0),
               {ok, Config} = mailfilter:get_config(MR),
               ?_assertMatch([{x, _}], Config)
       end}}].

add_mail_test_() ->
    [{"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               ?_assertMatch({ok, _},
                             (mailfilter:add_mail(MS, <<"abc">>)))
       end}},
     {"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               ?_assertMatch({ok, _},
                             (mailfilter:add_mail(MS, <<"abc">>)))
       end}}].

get_config_test_() ->
    [{"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               mailfilter:default(MS,
                                  y,
                                  {simple, fun (_, _) -> {just, truth} end},
                                  0),
               {ok, MR} = mailfilter:add_mail(MS, "x"),
               ?_assertMatch({ok, [{y, _}]},
                             (mailfilter:get_config(MR)))
       end}}].

enough_test_() ->
    [{"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               {ok, MR} = mailfilter:add_mail(MS, "x"),
               ?_assertMatch(ok, (mailfilter:enough(MR)))
       end}}].

add_filter_test_() ->
    [{"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               {ok, MR} = mailfilter:add_mail(MS, "x"),
               mailfilter:add_filter(MR, x, simple(), none),
               {ok, Config} = mailfilter:get_config(MR),
               ?_assertMatch([{x, _Result}], Config)
       end}}].

simple_filter_test_() ->
    [{"Add simple filter",
      {setup,
       fun start_mail/0,
       fun (MR) ->
               mailfilter:add_filter(MR, x, simple(), none),
               ?_assertMatch({ok, [{x, _Result}]},
                             (mailfilter:get_config(MR)))
       end}}].

chain_filter_test_() ->
    [{"Add empty chain filter",
      {setup,
       fun start_mail/0,
       fun (MR) ->
               mailfilter:add_filter(MR, x, chain_empty(), none),
               ?_assertMatch({ok, [{x, _Result}]},
                             (mailfilter:get_config(MR)))
       end}},
     {"Add chain filter",
      {setup,
       fun start_mail/0,
       fun (MR) ->
               mailfilter:add_filter(MR, x, chain(), none),
               ?_assertMatch({ok, [{x, _Result}]},
                             (mailfilter:get_config(MR)))
       end}}].

group_filter_test_() ->
    [{"Add empty chain filter",
      {setup,
       fun start_mail/0,
       fun (MR) ->
               mailfilter:add_filter(MR, x, group_empty(), none),
               ?_assertMatch({ok, [{x, _Result}]},
                             (mailfilter:get_config(MR)))
       end}},
     {"Add chain filter",
      {setup,
       fun start_mail/0,
       fun (MR) ->
               mailfilter:add_filter(MR, x, group(), none),
               ?_assertMatch({ok, [{x, _Result}]},
                             (mailfilter:get_config(MR)))
       end}}].

timelimit_filter_test_() ->
    [{"Add timelimit filter",
      {setup,
       fun start_mail/0,
       fun (MR) ->
               mailfilter:add_filter(MR, x, timelimit(1), none),
               ?_assertMatch({ok, [{x, _Result}]},
                             (mailfilter:get_config(MR)))
       end}},
     {"Add infinity timelimit filter",
      {setup,
       fun start_mail/0,
       fun (MR) ->
               mailfilter:add_filter(MR, x, timelimit(infinity), none),
               ?_assertMatch({ok, [{x, _Result}]},
                             (mailfilter:get_config(MR)))
       end}}].

start() ->
    {ok, MS} = mailfilter:start(infinite),
    MS.

start_mail() ->
    {ok, MS} = mailfilter:start(infinite),
    {ok, MR} = mailfilter:add_mail(MS, "x"),
    MR.

simple() ->
    {simple, fun (_, _) -> {just, something} end}.

chain_empty() -> {chain, []}.

chain() -> {chain, [simple(), simple()]}.

merge() ->
    fun (Results) ->
            case lists:any(fun (X) -> X =:= inprogress end, Results)
                of
                true -> continue;
                false -> {just, merged}
            end
    end.

group_empty() -> {group, [], merge()}.

group() -> {group, [simple(), simple()], merge()}.

timelimit(Time) -> {timelimit, Time, simple()}.

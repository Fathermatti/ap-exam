-module(test_analyzer).

-export([test_all/0]).

-export([]).

-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(register_test2_(), [verbose]).

-record(mail, {mail, inprogress, done}).

register_test_() ->
    [{"Hey manner",
      fun () ->
              M = <<"Remember to read AP exam text carefully">>,
              D = #{importance => {{simple, fun importance/2}, #{}}},
              ?assertMatch({ok, _}, (analyzer:new(M, D)))
      end}].

register_test2_() ->
    [{"Hey manner",
      fun () ->
              M = <<"Remember to read AP exam text carefully">>,
              D = #{importance => {{simple, fun importance/2}, #{}}},
              R = analyzer:new(M, D),
              ?assertMatch({ok, _}, R)
      end}].

register4_test_() ->
    [{"Hey manner",
      fun () ->
              M = <<"Remember to read AP exam text carefully">>,
              S = #mail{mail = M, done = #{},
                        inprogress = #{x => {{simple, fun importance/2}, #{}}}},
              R = analyzer:analyzing(internal, execute, S),
              ?assertMatch({next_state, done, _}, R)
      end}].

register5_test_() ->
    [{"Hey manner",
      fun () ->
              M = <<"Remember to read AP exam text carefully">>,
              S = #mail{mail = M, done = #{},
                        inprogress = #{x => {{simple, fun importance/2}, #{}}}},
              {next_state, done, Actual} =
                  analyzer:analyzing(internal, execute, S),
              ?assertEqual(0, (maps:size(Actual#mail.inprogress))),
              ?assertMatch((#{x := {{{simple, _}, #{}}, true}}),
                           (Actual#mail.done))
      end}].

register6_test_() ->
    [{"Filters remaining returns internal event",
      fun () ->
              M = <<"Remember to read AP exam text carefully">>,
              S = #mail{mail = M, done = #{},
                        inprogress =
                            #{x => {{simple, fun importance/2}, #{}},
                              y => {{simple, fun importance/2}, #{}}}},
              A = analyzer:analyzing(internal, execute, S),
              ?assertMatch({keep_state,
                            _,
                            [{next_event, internal, execute}]},
                           A),
              {keep_state, St, [{next_event, internal, execute}]} = A,
              ?assertEqual(1, (maps:size(St#mail.inprogress))),
              ?assertMatch((#{x := {{{simple, _}, #{}}, true}}),
                           (St#mail.done))
      end}].

register7_test_() ->
    [{"Filters remaining returns internal event",
      fun () ->
              F = {simple, fun importance/2},
              M = <<"Message">>,
              D = #{},
              ?assertEqual({just, true}, (analyzer:eval(F, M, D)))
      end},
     {"Filters remaining returns internal event",
      fun () ->
              A = {simple, app("C")},
              B = {simple, app("B")},
              C = {simple, app("A")},
              Chain = {chain, [A, B, C]},
              M = <<"Message">>,
              D = [],
              ?assertEqual({just, ["A", "B", "C"]},
                           (analyzer:eval(Chain, M, D)))
      end}].

register8_test_() ->
    [{"Filters remaining returns internal event",
      fun () ->
              A = {simple, fun importance/2},
              B = {simple, fun importance/2},
              C = {simple, fun importance/2},
              Chain = {group, [A, B, C], fun cont/1},
              M = <<"Message">>,
              D = #{},
              ?assertEqual({just, true}, (analyzer:eval(Chain, M, D)))
      end}].

importance(M, C) -> {just, true}.

cont(X) ->
    case length(lists:filter(fun (Y) -> Y =:= inprogress
                             end,
                             X))
        of
        0 -> {just, true};
        _ -> continue
    end.

app(X) -> fun (M, C) -> {just, [X | C]} end.

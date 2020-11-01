-module(test_mailanalyzer).

-export([]).

-include_lib("eunit/include/eunit.hrl").
 

stop_test_() ->
    [{"Stop running server",
      fun () ->
              R = mailanalyzer:evaluate(pid, {simple, fun (_, _) -> {just, x} end}, mail, data),
              ?assertMatch({just,x}, R)
      end}].

chain_test_() ->
    [{"Stop running server",
      fun () ->
              R = mailanalyzer:evaluate(pid, {chain, []}, mail, data),
              ?assertMatch(unchanged, R)
      end},
      {"Stop running server",
      fun () ->
          {ok, MS} = mailserver:start(infinite),
              R = mailanalyzer:evaluate(MS, {chain, [{simple, fun (_, _) -> {just, x} end}]}, mail, data),
              ?assertMatch({just,hey}, R)
      end}].

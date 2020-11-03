-module(test_filterserver).

-export([]).

-include_lib("eunit/include/eunit.hrl").
 

stop_test_() ->
    [{"Stop running server",
      fun () ->
              R = filter:evaluate(pid, {simple, fun (_, _) -> {just, x} end}, mail, data),
              ?assertMatch({just,x}, R)
      end}].

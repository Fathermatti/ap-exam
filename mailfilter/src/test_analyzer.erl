-module(test_analyzer).

-export([test_all/0]).

-export([]).

-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(register_test2_    (), [verbose]).

-record(mail, {mail, inprogress, done}).

register_test_() ->
    [{"Hey manner",
      fun () ->
         M =  <<"Remember to read AP exam text carefully">>,
         D = #{importance => {{simple, fun importance/2}, #{}}},
          ?assertMatch({ok, _}, analyzer:new(M, D))
      end
    }].

register_test2_() ->
    [{"Hey manner",
      fun () ->
         M =  <<"Remember to read AP exam text carefully">>,
         D = #{importance => {{simple, fun importance/2}, #{}}},
         R = analyzer:new(M, D),
          ?assertMatch({ok, _}, R)
      end
    }].

register2_test_() ->
    [{"Hey manner",
      fun () ->
         R = analyzer:analyzing(internal, execute, #mail{inprogress = #{}}),
          ?assertMatch({next_state, done, _}, R)
      end
    }].

register4_test_() ->
    [{"Hey manner",
      fun () ->
         M =  <<"Remember to read AP exam text carefully">>,
         S = #mail{mail = M, done = #{}, inprogress = #{x => {{simple, fun importance/2}, #{}}}},
         R = analyzer:analyzing(internal, execute, S),
          ?assertMatch({next_state, done, _}, R)
      end
    }].

register5_test_() ->
    [{"Hey manner",
      fun () ->
         M =  <<"Remember to read AP exam text carefully">>,
         S = #mail{mail = M, done = #{}, inprogress = #{x => {{simple, fun importance/2}, #{}}}},

         {next_state, done, Actual} = analyzer:analyzing(internal, execute, S),

          ?assertEqual(0, maps:size(Actual#mail.inprogress)),
          ?assertMatch(#{x := _ }, Actual#mail.done)
      end
    }].

importance(M, C) ->
    {just, true}.

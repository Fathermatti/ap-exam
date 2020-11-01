-module(test_mailfilter).

-export([]). % Remember to export the other function from Q2.2

% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_mailfilter.
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
              ?assertMatch({ok, []}, (mailfilter:stop(MS)))
      end},
     {"Stop nonrunning server",
      fun () ->
              ?assertMatch({error, _}, (mailfilter:stop(pid)))
      end}].

add_mail_test_() ->
    [{"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               [?_assertMatch({ok, _},
                              (mailfilter:add_mail(MS, <<"abc">>)))]
       end}},
     {"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               [?_assertMatch({ok, _},
                              (mailfilter:add_mail(MS, <<"abc">>)))]
       end}}].

get_config_test_() ->
    [{"Add mail to empty mail server",
      {setup,
       fun start/0,
       fun (MS) ->
               mailfilter:default(MS, x, {simple, fun(_, _) -> {just, truth} end}, 0),
               {ok, MR} = mailfilter:add_mail(MS, <<"abc">>),
               [?_assertMatch({<<"abc">>,[{x,{done,truth}}]}, (mailfilter:get_config(MR)))]
       end}}].

start() ->
    {ok, Pid} = mailfilter:start(infinite),
    Pid.

-module(test_mailfilter).

-export([test_all/0, test_everything/0]).

-export([]). % Remember to export the other function from Q2.2

% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_mailfilter.
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(register_test_(), [verbose]).

start() -> ok.

stop(_) -> ok.

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

register_test_() ->
    [{"Hey manner",
      fun () ->
              ?assertMatch({ok, _}, (mailfilter:start(infinite)))
      end},
     {"Hey manner",
      fun () ->
            {ok, MS} = mailfilter:start(infinite),
              ?assertMatch({ok, _}, mailfilter:add_mail(MS, some_mail))
      end},
     {"Hey manner",
      fun () ->
            {ok, MS} = mailfilter:start(infinite),
            {ok, MR} = mailfilter:add_mail(MS, some_mail),
              ?assert(is_reference(MR))
      end}].

test_everything() -> test_all().

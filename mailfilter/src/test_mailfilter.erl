-module(test_mailfilter).

-export([test_all/0, test_everything/0]).
-export([]). % Remember to export the other function from Q2.2

test_all() ->
  eunit:test(test_mailfilter_eunit),
  eqc:module(test_mailfilter_qc).

test_everything() ->
  test_all().

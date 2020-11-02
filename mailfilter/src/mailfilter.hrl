-type mail() :: any().

-type data() :: any().

-type label() :: any().

-type result() :: {done, data()} | inprogress.

-type labelled_result() :: {label(), result()}.

-type filter_result() :: {just, data()} |
                         {transformed, mail()} |
                         unchanged |
                         {both, mail(), data()}.

-type filter_fun() :: fun((mail(),
                           data()) -> filter_result()).

-type filter() :: {simple, filter_fun()} |
                  {chain, [filter()]} |
                  {group, [filter()], merge_fun()} |
                  {timelimit, timeout(), filter()}.

-type merge_fun() :: fun(([filter_result() |
                           inprogress]) -> filter_result() | continue).

-type configuration() :: [labelled_result()].

-type capacity() :: integer() | infinite.
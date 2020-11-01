-module(mailfilter).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called mailfilter.

% Export at least the API:
-export([add_filter/4,
         add_mail/2,
         default/4,
         enough/1,
         get_config/1,
         start/1,
         stop/1]).

start(Cap) -> mailserver:start(Cap).

stop(MS) ->
    try mailserver:stop(MS) of
        State -> {ok, State}
    catch
        exit:Error -> {error, Error}
    end.

add_mail(MS, Mail) -> mailserver:add_mail(MS, Mail).

get_config(MR) -> mailanalyzer:get_config(MR).

default(MS, Label, Filt, Data) ->
    mailserver:add_filter(MS, Label, Filt, Data).

enough(MR) -> mailanalyzer:stop(MR).

add_filter(MR, Label, Filt, Data) ->
    mailanalyzer:add_filter(MR, Label, Filt, Data).

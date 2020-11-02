-module(mailfilter).

-export([add_filter/4,
         add_mail/2,
         default/4,
         enough/1,
         get_config/1,
         start/1,
         stop/1]).

start(Cap) -> 
    case filterserver:start(Cap) of 
        {ok, FS} -> mailserver:start(FS);
        Error -> Error
    end.

stop(MS) ->
    try mailserver:stop(MS) of
        State -> {ok, State}
    catch
        exit:Error -> {error, Error}
    end.

default(MS, Label, Filt, Data) ->
    mailserver:add_filter(MS, Label, Filt, Data).

add_mail(MS, Mail) -> mailserver:add_mail(MS, Mail).

get_config(MR) -> {ok, mailanalyzer:get_config(MR)}.

enough(MR) -> mailanalyzer:close(MR).

add_filter(MR, Label, Filt, Data) ->
    mailanalyzer:add_filter(MR, Label, Filt, Data).

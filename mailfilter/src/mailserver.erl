-module(mailserver).

% Public API
-export([add_filter/4, add_mail/2, start/1, stop/1]).

% Callback functions
-export([handle_call/3, handle_cast/2, init/1]).

-behaviour(gen_server).

-record(state, {fs, mails = [], defaults = #{}}).

start(FS) -> gen_server:start(?MODULE, FS, []).

stop(MS) -> gen_server:call(MS, stop).

add_mail(MS, Mail) ->
    gen_server:call(MS, {add_mail, Mail}).

add_filter(MS, Label, Filt, Data) ->
    gen_server:cast(MS, {add_filter, Label, Filt, Data}).

init(FS) -> {ok, #state{fs = FS}}.

handle_call({add_mail, Mail}, _From,
            #state{fs = FS, mails = Mails, defaults = Defs} = S) ->
    case mailanalyzer:new(self(), FS, Mail, Defs) of
        {ok, MR} ->
            {reply, {ok, MR}, S#state{mails = [MR | Mails]}};
        Err -> {reply, Err, S}
    end;
handle_call(stop, _From, S = #state{mails = Mails}) ->
    {stop,
     normal,
     lists:map(fun (MR) -> mailanalyzer:complete(MR) end,
               Mails),
     S}.

handle_cast({add_filter, Label, Filt, Data},
            #state{defaults = Defs} = S) ->
    A = case maps:is_key(Label, Defs) of
            true -> Defs;
            false -> Defs#{Label => {Filt, Data}}
        end,
    {noreply, S#state{defaults = A}};
handle_cast({remove_mail, MR}, S = #state{mails = M}) ->
    {noreply, S#state{mails = lists:delete(MR, M)}}.

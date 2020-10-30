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

-record(state, {filts = #{}, mails = #{}}).

-type ms() :: pid().

% You may have other exports as well
-export([]).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).

start() ->
    case gen_server:start(?MODULE, [], []) of
        ignore -> {error, "ignored"};
        Res -> Res
    end.

init(_Args) -> {ok, #state{}}.

handle_call({add, Mail}, _From, S = #state{mails = Mails, filts = Filts}) -> 
  Ref = make_ref(),
  {reply, {ok, Ref}, S#state{mails = maps:put(Ref, {Mail, Filts}, Mails)}};
handle_call({config, Ref}, _From, S = #state{mails = Mails}) -> 
  {Mail, Filts} = maps:get(Ref, Mails),
  M = maps:map(fun(F) -> apply(F, Mail) end, Filts), 
  {reply, {ok, maps:to_list(M)}, S}.

handle_cast({def, Label, Filt, Data},
            S = #state{filts = Filts}) ->
    NewFilts = case maps:is_key(Label, Filts) of
                   true -> maps:put(Label, Filt, Filts);
                   false -> Filts
               end,
    {noreply, S#state{filts = NewFilts}}.

apply({simple, Fun}, Mail, Data) -> 
  Fun(Mail, Data).  



% API :

start(_Cap) -> start().

stop(_MS) -> not_implemented.

add_mail(MS, Mail) ->
    gen_server:call(MS, {add, Mail}).

get_config(MR) ->
    gen_server:call(MS, {config, MR}).

default(MS, Label, Filt, Data) ->
    gen_server:cast(MS, {def, Label, Filt, Data}).

enough(_MR) -> not_implemented.

add_filter(_MR, _Label, _Filt, _Data) ->
    not_implemented.

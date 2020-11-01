-module(mailserver).

% Public API
-export([add_filter/4,
         add_mail/2,
         execute/2,
         start/1,
         stop/1]).

% Callback functions
-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1]).

-behaviour(gen_server).

-record(state,
        {cap,
         active = 0,
         queue = [],
         mails = [],
         defaults = #{}}).

start(infinite) -> new(infinite);
start(Cap) when is_integer(Cap), Cap >= 1 -> new(Cap);
start(_) -> {error, invalid_capacity}.

new(Cap) -> gen_server:start(?MODULE, Cap, []).

stop(MS) -> gen_server:call(MS, stop).

add_mail(MS, Mail) ->
    gen_server:call(MS, {add_mail, Mail}).

add_filter(MS, Label, Filt, Data) ->
    gen_server:cast(MS, {add_filter, Label, Filt, Data}).

execute(MS, Fun) -> gen_server:cast(MS, {execute, Fun}).

init(Cap) -> {ok, #state{cap = Cap}}.

handle_call({add_mail, Mail}, _From,
            #state{mails = Mails, defaults = Defs} = S) ->
    case mailanalyzer:new(self(), Mail, Defs) of
        {ok, MR} ->
            {reply, {ok, MR}, S#state{mails = [MR | Mails]}};
        Otherwise -> {reply, Otherwise, S}
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
handle_cast({execute, Fun},
            #state{cap = Cap, active = Active, queue = Queue} =
                S) ->
    if Active < Cap ->
           io:fwrite("Hello world!~s~n", [Cap]),
           execute(Fun),
           {noreply, S#state{active = Active + 1}};
       Active =:= Cap ->
           {noreply, S#state{queue = Queue ++ [Fun]}}
    end;
handle_cast({remove_mail, MR}, S = #state{mails = M}) ->
    {noreply, S#state{mails = lists:delete(MR, M)}}.

handle_info({'DOWN', _, _, _, _},
            #state{active = Active, queue = Queue} = S) ->
    case Queue of
        [] -> {noreply, S#state{active = Active - 1}};
        [X | XS] ->
            executor:start(self(), X),
            {noreply, S#state{queue = XS}}
    end;
handle_info(X, _S) ->
    io:fwrite("Hello world!~s~n", [X]).

execute(Func) ->
    spawn_monitor(fun () ->
                          Func(),
                          io:fwrite("GG!~n", [])
                  end).

-module(mailanalyzer).

-behaviour(gen_statem).

% Public API
-export([add_filter/4,
         close/1,
         complete/1,
         get_config/1,
         new/4]).

% Callback API
-export([active/3,
         callback_mode/0,
         inactive/3,
         init/1]).

-record(state, {ref, ms, fs, mail, running, done}).

new(MS, FS, Mail, Filters) ->
    gen_statem:start(?MODULE, {MS, FS, Mail, Filters}, []).

add_filter(MR, Label, Filt, Data) ->
    gen_statem:cast(MR, {add, Label, Filt, Data}).

get_config(MR) -> gen_statem:call(MR, config).

close(MR) -> gen_statem:cast(MR, close).

complete(MR) -> gen_statem:call(MR, complete).

callback_mode() -> state_functions.

init({MS, FS, Mail, Filters}) ->
    Ref = make_ref(),
    run(FS, Ref, Mail, Filters),
    {ok,
     active,
     #state{ref = Ref, ms = MS, mail = Mail,
            running = Filters, done = #{}}}.

active(cast, {done, Ref, Label, {just, Data}},
       #state{ref = Ref} = S) ->
    {keep_state, commit(S, Label, Data)};
active(cast, {done, Ref, Label, unchanged},
       S = #state{ref = Ref}) ->
    {keep_state, unchanged(S, Label)};
% active(cast, {done, Ref, Label, {transformed, Mail}},
%        S = #state{ref = Ref}) ->
%     {keep_state, transform(S, Label)};
active({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, configuration(S)}]};
active({call, From}, complete, S) ->
    {next_state, inactive, S, [{reply, From, result(S)}]};
active(cast, close, S) -> {next_state, inactive, S}.

% active(cast, {add, Label, Filt, Data},
%           #state{ms = MS, running = Running, done = Done} = S) ->
%     case lists:any(fun (X) -> X =:= Label end, Running) of
%         true -> {keep_state, S};
%         false ->
%             mailserver:execute(MS,
%                                fun () -> io:fwrite("Hello world 2~n", []) end),
%             {keep_state,
%              S#state{running = [{Label, Filt, Data} | Running]}}
%     end;

inactive(cast, {add, _, _, _}, _S) ->
    {keep_state_and_data};
inactive(cast, close, _S) -> {keep_state_and_data};
inactive({call, From}, complete, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]};
inactive({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, configuration(S)}]}.

configuration(#state{running = Running,
              done = Done}) ->
     maps:values(maps:map(fun (Label, {_, _}) ->
                                  {Label, inprogress}
                          end,
                          Running))
         ++
         maps:values(maps:map(fun (Label, {_, Data}) ->
                                      {Label, {done, Data}}
                              end,
                              Done)).

result(#state{mail = Mail, running = Running,
              done = Done}) ->
    {Mail,
     maps:values(maps:map(fun (Label, {_, _}) ->
                                  {Label, inprogress}
                          end,
                          Running))
         ++
         maps:values(maps:map(fun (Label, {_, Data}) ->
                                      {Label, {done, Data}}
                              end,
                              Done))}.

callback(MR, Ref, Label) ->
    fun (Res) ->
            gen_statem:cast(MR, {done, Ref, Label, Res})
    end.

run(FS, Ref, Mail, Filters) ->
    maps:map(execute(FS, Ref, Mail), Filters).

execute(FS, Ref, Mail) ->
    Me = self(),
    fun (Label, {Filt, Data}) ->
            Call = callback(Me, Ref, Label),
            filterserver:run(FS, {Mail, Filt, Data, Call})
    end.

commit(#state{running = R, done = D} = S, Label,
       Data) ->
    case maps:take(Label, R) of
        {Running, Rest} ->
            S#state{running = Rest,
                    done = D#{Label => {Running, Data}}};
        error -> S
    end.

unchanged(#state{running = R, done = D} = S, Label) ->
    case maps:take(Label, R) of
        {{Filter, Data}, Rest} ->
            S#state{running = Rest,
                    done = D#{Label => {{Filter, Data}, Data}}};
        error -> S
    end.

transform(#state{running = R, done = D} = S, Label,
          Mail) ->
    case maps:take(Label, R) of
        {{Filter, Data}, Rest} ->
            Re = maps:map(fun (Label, {{Filter, Data}, _}) ->
                                  {Filter, Data}
                          end,
                          D),
            S#state{mail = Mail, running = Re,
                    done = #{Label => {{Filter, Data}, Data}}};
        error -> S
    end.

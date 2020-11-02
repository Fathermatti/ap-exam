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

-include("mailfilter.hrl").

-record(state,
        {ms :: pid(),
         fs :: pid(),
         ref :: reference(),
         mail :: mail(),
         inprogress :: #{label() := {filter(), data()}},
         done :: #{label() := {filter(), data(), data()}}}).

new(MS, FS, Mail, Filters) ->
    gen_statem:start(?MODULE, {MS, FS, Mail, Filters}, []).

add_filter(MR, Label, Filt, Data) ->
    gen_statem:cast(MR, {add, Label, Filt, Data}).

get_config(MR) -> gen_statem:call(MR, config).

close(MR) -> gen_statem:cast(MR, close).

complete(MR) -> gen_statem:call(MR, complete).

callback_mode() -> state_functions.

init({MS, FS, Mail, Regs}) ->
    Ref = make_ref(),
    run(FS, Ref, Mail, Regs),
    {ok,
     active,
     #state{ms = MS, fs = FS, ref = Ref, mail = Mail,
            inprogress = Regs, done = #{}}}.

active(cast, {add, Label, Filt, Data}, S) ->
    {keep_state, add(S, Label, Filt, Data)};
active(cast, {result, Ref, Label, {just, Data}},
       #state{ref = Ref} = S) ->
    {keep_state, just(S, Label, Data)};
active(cast, {result, Ref, Label, unchanged},
       S = #state{ref = Ref}) ->
    {keep_state, unchanged(S, Label)};
active(cast, {result, Ref, Label, {transformed, Mail}},
       S = #state{ref = Ref}) ->
    {keep_state, transform(S, Label, Mail)};
active(cast, {result, Ref, Label, {both, Mail, Data}},
       S = #state{ref = Ref}) ->
    {keep_state, both(S, Label, Mail, Data)};
active(cast, {result, _Ref, _Label, _Result}, _S) ->
    keep_state_and_data;
active({call, From}, config, S) ->
    {keep_state_and_data,
     [{reply, From, configuration(S)}]};
active({call, From}, complete, S) ->
    {next_state, inactive, S, [{reply, From, state(S)}]};
active(cast, close, #state{ms = MS} = S) ->
    mailserver:remove(MS, self()),
    {next_state, inactive, S}.

inactive(cast, {add, _, _, _}, _S) ->
    keep_state_and_data;
inactive(cast, {result, _, _, _}, _S) ->
    keep_state_and_data;
inactive(cast, close, #state{ms = MS}) -> 
     mailserver:remove(MS, self()),
     keep_state_and_data;
inactive({call, From}, complete, S) ->
    {keep_state_and_data, [{reply, From, state(S)}]};
inactive({call, From}, config, S) ->
    {keep_state_and_data,
     [{reply, From, configuration(S)}]}.

state(#state{mail = Mail} = S) ->
    {Mail, configuration(S)}.

configuration(#state{inprogress = I, done = D}) ->
    Prog = [{Label, inprogress}
            || {Label, _Reg} <- maps:to_list(I)],
    Done = [{Label, {done, Result}}
            || {Label, {_Filt, _Data, Result}} <- maps:to_list(D)],
      Prog ++ Done.

callback(MR, Ref, Label) ->
    fun (Res) ->
            gen_statem:cast(MR, {result, Ref, Label, Res})
    end.

run(FS, Ref, Mail, Regs) ->
    maps:map(fun (Label, {Filt, Data}) ->
                     run(FS, Ref, Label, Mail, Filt, Data)
             end,
             Regs).

run(FS, Ref, Label, Mail, Filt, Data) ->
    Callback = callback(self(), Ref, Label),
    filterserver:run(FS, {Mail, Filt, Data, Callback}).

just(#state{inprogress = I, done = D} = S, Label,
     Result) ->
    case maps:take(Label, I) of
        {{Filt, Data}, Rest} ->
            S#state{inprogress = Rest,
                    done = D#{Label => {Filt, Data, Result}}};
        error -> S
    end.

unchanged(#state{inprogress = R, done = D} = S,
          Label) ->
    case maps:take(Label, R) of
        {{Filter, Data}, Rest} ->
            S#state{inprogress = Rest,
                    done = D#{Label => {Filter, Data, Data}}};
        error -> S
    end.

transform(#state{fs = FS, inprogress = I, done = D} = S,
          Label, Mail) ->
    case maps:take(Label, I) of
        {{Filt, Data}, Rest} ->
            Rerun = maps:merge(Rest, registrations(D)),
            Ref = make_ref(),
            run(FS, Ref, Mail, Rerun),
            S#state{mail = Mail, inprogress = Rerun,
                    done = #{Label => {Filt, Data, Data}}};
        error -> S
    end.

both(#state{fs = FS, inprogress = I, done = D} = S,
     Label, Mail, Result) ->
    case maps:take(Label, I) of
        {{Filt, Data}, Rest} ->
            Rerun = maps:merge(Rest, registrations(D)),
            Ref = make_ref(),
            run(FS, Ref, Mail, Rerun),
            S#state{mail = Mail, inprogress = Rerun,
                    done = #{Label => {Filt, Data, Result}}};
        error -> S
    end.

registrations(Done) ->
    maps:map(fun (_Label, {Filt, Data, _Res}) ->
                     {Filt, Data}
             end,
             Done).

add(#state{mail = Mail, fs = FS, ref = Ref,
           inprogress = I, done = D} =
        S,
    Label, Filt, Data) ->
    case maps:is_key(Label, I) or maps:is_key(Label, D) of
        true -> S;
        false ->
            run(FS, Ref, Label, Mail, Filt, Data),
            S#state{inprogress = I#{Label => {Filt, Data}}}
    end.

-module(analyzer).

-behaviour(gen_statem).

% Public API
-export([new/2]).

% Callback API
-export([analyzing/3, callback_mode/0, done/3, init/1]).

-record(mail, {mail, inprogress, done}).

new(Mail, Filts) ->
    case gen_statem:start(?MODULE, {Mail, Filts}, []) of
        ignore -> {error, ignore};
        {error, Err} -> {error, Err};
        {ok, Pid} ->
            gen_statem:cast(Pid, start),
            {ok, Pid}
    end.

callback_mode() -> state_functions.

init({Mail, Filts}) ->
    {ok,
     analyzing,
     #mail{mail = Mail, inprogress = Filts, done = #{}}}.

analyzing({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]};
analyzing(cast, start, S) ->
    {keep_state, S, [{next_event, internal, execute}]};
analyzing(internal, execute, S) ->
    New = evaluate(S),
    case New#mail.inprogress of
        #{} -> {next_state, done, S};
        _ ->
            {keep_state, New, [{next_event, internal, execute}]}
    end.

done({call, From}, config, S) ->
    {keep_state_and_data, [{reply, From, result(S)}]}.

result(#mail{inprogress = I, done = D}) ->
    II = maps:map(fun (_K, _) -> inprogress end, I),
    DD = maps:map(fun (_K, {_, Data}) -> {done, Data} end,
                  D),
    maps:to_list(II) ++ maps:to_list(DD).

next(Map) ->
    I = maps:iterator(Map),
    {Label, _, _} = maps:next(I),
    Label.

evaluate(S = #mail{mail = Mail, inprogress = IP,
                   done = Done}) ->
    Label = next(IP),
    case maps:take(Label, IP) of
        {{Filt, Data}, New} ->
            Res = eval(Filt, Mail, Data),
            {_M, D} = decide(Mail, Data, Res),
            S#mail{inprogress = New,
                   done = maps:put(Label, {{Filt, Data}, D}, Done)};
        error -> S
    end.

eval(Filt, Mail, Data) ->
    case Filt of 
        {simple, Fun} -> Fun(Mail, Data);
    _ -> throw(shiiieeeeet)
    end.


decide(Mail, Data, Res) ->
    case Res of
        unchanged -> {Mail, Data};
        {transformed, M} -> {M, Data};
        {just, D} -> {Mail, D};
        {both, M, D} -> {M, D}
    end.

% second({call, From}, stop,

%        {{Other, _OtherChoice}, _Game}) ->
%     Replies = [{reply, From, stopped},
%                {reply, Other, server_stopping}],
%     {next_state, stopping, [Other], Replies};
% second({call, From}, Choice,
%        {{OtherFrom, OtherChoice}, Game}) ->
%     {Result, OtherResult} = play(From,
%                                  Choice,
%                                  OtherFrom,
%                                  OtherChoice),
%     NewGame = standing(Result, OtherResult, Game),
%     case is_game_over(NewGame) of
%         false ->
%             Replies = reply_results(Result, OtherResult),
%             {next_state, first, NewGame, Replies};
%         true ->
%             gen_event:notify(NewGame#game.broker,
%                              {done, self(), NewGame#game.rounds}),
%             Replies = reply_game_over(From, OtherFrom, NewGame),
%             {stop_and_reply, done, Replies}
%     end.

% stopping({call, From}, _Choice, []) ->
%     {keep_state, [From], [{reply, From, server_stopping}]};
% stopping({call, From}, _Choice, [_Other]) ->
%     {stop_and_reply,
%      stopped,
%      [{reply, From, server_stopping}]}.

% reply_results({From, Result}, {OtherFrom, OtherResult}) ->
%     [{reply, From, Result}, {reply, OtherFrom, OtherResult}].

% reply_game_over(From = {Player, _}, OtherFrom = {Other, _}, Game) ->
%     P = maps:get(Player, Game#game.standing),
%     O = maps:get(Other, Game#game.standing),
%     [{reply, From, {game_over, P, O}},
%      {reply, OtherFrom, {game_over, O, P}}].

% is_game_over(Game) ->
%     Max = lists:max(maps:values(Game#game.standing)),
%     Limit = Game#game.rounds / 2,
%     Max > Limit.

% standing({{Player, _Tag}, Result}, {{Other, _OtherTag}, _OtherResult}, Game) ->
%     case Result of
%         round_won -> won(Player, Game);
%         round_lost -> won(Other, Game);
%         tie -> Game
%     end.

% won(Player, Game) ->
%     S = maps:update_with(Player,
%                          fun (X) -> X + 1 end,
%                          Game#game.standing),
%     Game#game{standing = S}.

% play(From, Choice, Other, OtherChoice) ->
%     {Outcome, OtherOutcome} = decide(Choice, OtherChoice),
%     {{From, Outcome}, {Other, OtherOutcome}}.

% decide(Choice, OtherChoice) ->
%     M = #{rock => 1, scissor => 2, paper => 3},
%     This = maps:get(Choice, M, nothing),
%     That = maps:get(OtherChoice, M, nothing),
%     case {This, That} of
%         {X, X} -> {tie, tie};
%         {nothing, _} -> {round_lost, round_won};
%         {_, nothing} -> {round_won, round_lost};
%         {X, Y} when (X + 1) rem 3 == Y ->
%             {round_won, round_lost};
%         _ -> {round_lost, round_won}
%     end.


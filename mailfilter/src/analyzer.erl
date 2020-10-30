-module(coordinator).

-behaviour(gen_statem).

% Public API
-export([new/3, move/2, stop/1]).

% Callback API
-export([callback_mode/0,
         init/1,
         first/3,
         second/3,
         stopping/3,
         is_game_over/1]).

-record(game, {standing, rounds, broker}).

new(BrokerRef, Rounds, {Player, Other}) ->
    gen_statem:start(?MODULE,
                     {BrokerRef, Rounds, {Player, Other}},
                     []).

move(Coordinator, Choice) ->
    gen_statem:call(Coordinator, Choice).

stop(Coordinator) ->
    gen_statem:call(Coordinator, stop).

callback_mode() -> state_functions.

init({BrokerRef, Rounds, {Player, Other}}) ->
    {ok,
     first,
     #game{standing = #{Player => 0, Other => 0},
           rounds = Rounds, broker = BrokerRef}}.

first({call, From}, stop, _Game) ->
    {next_state, stopping, [], [{reply, From, stopped}]};
first({call, From}, Choice, Game) ->
    {next_state, second, {{From, Choice}, Game}}.

second({call, From}, stop,
       {{Other, _OtherChoice}, _Game}) ->
    Replies = [{reply, From, stopped},
               {reply, Other, server_stopping}],
    {next_state, stopping, [Other], Replies};
second({call, From}, Choice,
       {{OtherFrom, OtherChoice}, Game}) ->
    {Result, OtherResult} = play(From,
                                 Choice,
                                 OtherFrom,
                                 OtherChoice),
    NewGame = standing(Result, OtherResult, Game),
    case is_game_over(NewGame) of
        false ->
            Replies = reply_results(Result, OtherResult),
            {next_state, first, NewGame, Replies};
        true ->
            gen_event:notify(NewGame#game.broker,
                             {done, self(), NewGame#game.rounds}),
            Replies = reply_game_over(From, OtherFrom, NewGame),
            {stop_and_reply, done, Replies}
    end.

stopping({call, From}, _Choice, []) ->
    {keep_state, [From], [{reply, From, server_stopping}]};
stopping({call, From}, _Choice, [_Other]) ->
    {stop_and_reply,
     stopped,
     [{reply, From, server_stopping}]}.

reply_results({From, Result}, {OtherFrom, OtherResult}) ->
    [{reply, From, Result}, {reply, OtherFrom, OtherResult}].

reply_game_over(From = {Player, _}, OtherFrom = {Other, _}, Game) ->
    P = maps:get(Player, Game#game.standing),
    O = maps:get(Other, Game#game.standing),
    [{reply, From, {game_over, P, O}},
     {reply, OtherFrom, {game_over, O, P}}].

is_game_over(Game) ->
    Max = lists:max(maps:values(Game#game.standing)),
    Limit = Game#game.rounds / 2,
    Max > Limit.

standing({{Player, _Tag}, Result}, {{Other, _OtherTag}, _OtherResult}, Game) ->
    case Result of
        round_won -> won(Player, Game);
        round_lost -> won(Other, Game);
        tie -> Game
    end.

won(Player, Game) ->
    S = maps:update_with(Player,
                         fun (X) -> X + 1 end,
                         Game#game.standing),
    Game#game{standing = S}.

play(From, Choice, Other, OtherChoice) ->
    {Outcome, OtherOutcome} = decide(Choice, OtherChoice),
    {{From, Outcome}, {Other, OtherOutcome}}.

decide(Choice, OtherChoice) ->
    M = #{rock => 1, scissor => 2, paper => 3},
    This = maps:get(Choice, M, nothing),
    That = maps:get(OtherChoice, M, nothing),
    case {This, That} of
        {X, X} -> {tie, tie};
        {nothing, _} -> {round_lost, round_won};
        {_, nothing} -> {round_won, round_lost};
        {X, Y} when (X + 1) rem 3 == Y ->
            {round_won, round_lost};
        _ -> {round_lost, round_won}
    end.

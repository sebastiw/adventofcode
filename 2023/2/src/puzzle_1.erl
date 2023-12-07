-module(puzzle_1).

-export([run/1]).

-define(TARGET_RED, 12).
-define(TARGET_GREEN, 13).
-define(TARGET_BLUE, 14).

run(FileName) ->
    {ok, IODev} = file:open(FileName, [read]),
    loop_file(IODev, file:read_line(IODev), 0).

loop_file(IODev, eof, Acc) ->
    file:close(IODev),
    Acc;
loop_file(IODev, {ok, Data}, Acc) ->
    [Game|Sets] = string:tokens(Data, ":;"),
    G = string:substr(Game, 6),
    A = case check_game(Sets) of
            true ->
                Acc + list_to_integer(G);
            false ->
                Acc
        end,
    loop_file(IODev, file:read_line(IODev), A).

check_game(Sets) ->
    lists:all(fun check_set/1, Sets).

check_set(S) ->
    Gs = string:tokens(S, ","),
    {Red, Green, Blue} = check_colors(Gs, {0, 0, 0}),
    check(Red, Green, Blue).

check_colors([], Acc) ->
    Acc;
check_colors([G|Gs], {Red, Green, Blue}) ->
    Acc = case re:run(G, "([0-9]+) (red|green|blue)", [{capture, all_but_first, list}]) of
              {match, [N, "red"]} -> {list_to_integer(N)+Red, Green, Blue};
              {match, [N, "green"]} -> {Red, list_to_integer(N)+Green, Blue};
              {match, [N, "blue"]} -> {Red, Green, list_to_integer(N)+Blue}
          end,
    check_colors(Gs, Acc).

check(Red, Green, Blue) ->
    Red =< ?TARGET_RED andalso
    Green =< ?TARGET_GREEN andalso
    Blue =< ?TARGET_BLUE.

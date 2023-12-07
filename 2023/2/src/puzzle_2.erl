-module(puzzle_2).

-export([run/1]).

run(FileName) ->
    {ok, IODev} = file:open(FileName, [read]),
    loop_file(IODev, file:read_line(IODev), 0).

loop_file(IODev, eof, Acc) ->
    file:close(IODev),
    Acc;
loop_file(IODev, {ok, Data}, Acc) ->
    [Game|Sets] = string:tokens(Data, ":;"),
    G = string:substr(Game, 6),
    {Red, Green, Blue} = check_game(Sets),
    A = Red*Green*Blue,
    io:format("Game ~s: ~B~n", [G, A]),
    loop_file(IODev, file:read_line(IODev), Acc+A).

check_game(Sets) ->
    lists:foldl(fun (Set, Acc) ->
                        check_set(Set, Acc)
                end, {0, 0, 0}, Sets).

check_set(S, Acc) ->
    Cs = string:tokens(S, ","),
    check_colors(Cs, Acc).

check_colors([], Acc) ->
    Acc;
check_colors([C|Cs], {Red, Green, Blue}) ->
    Acc = case re:run(C, "([0-9]+) (red|green|blue)", [{capture, all_but_first, list}]) of
              {match, [N, "red"]} ->
                  case list_to_integer(N) of
                      R when R < Red ->
                          {Red, Green, Blue};
                      R ->
                          {R, Green, Blue}
                  end;
              {match, [N, "green"]} ->
                  case list_to_integer(N) of
                      G when G < Green ->
                          {Red, Green, Blue};
                      G ->
                          {Red, G, Blue}
                  end;
              {match, [N, "blue"]} ->
                  case list_to_integer(N) of
                      B when B < Blue ->
                          {Red, Green, Blue};
                      B ->
                          {Red, Green, B}
                  end
          end,
    check_colors(Cs, Acc).


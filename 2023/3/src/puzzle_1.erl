-module(puzzle_1).

-export([run/1]).

run(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    binary:split(Bin, <<"\n">>, [global, trim_all]).

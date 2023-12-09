-module(puzzle_1).

-export([run/1]).

run(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Ls = binary:split(Bin, <<"\n">>, [global, trim_all]),
    Lines = lists:zip(lists:seq(1, length(Ls)), [<<"..", L/binary, "..">> || L <- Ls]),
    Pos = find_all_symbols(Lines),
    lists:sum(find_all_integers(Lines, Pos)).

find_all_symbols(Ls) ->
    find_all_symbols(Ls, []).

find_all_symbols([], Acc) ->
    lists:flatten(lists:reverse(Acc));
find_all_symbols([{Line, L}|Ls], Acc) ->
    case re:run(L, "[^.0-9]", [global, {capture, all}]) of
        nomatch ->
            find_all_symbols(Ls, Acc);
        {match, Ms} ->
            Pos = [{Line, P} || M <- Ms,
                                {P, _} <- M],
            find_all_symbols(Ls, [Pos|Acc])
    end.

find_all_integers([], _) ->
    [];
find_all_integers([{LineNo, L}|Ls], Pos) ->
    Matches = find_matching_symbols(LineNo, Pos),
    [get_integers(L, Matches)|find_all_integers(Ls, Pos)].

find_matching_symbols(LineNo, Pos) ->
    Pre = proplists:get_all_values(LineNo-1, Pos),
    Same = proplists:get_all_values(LineNo, Pos),
    Post = proplists:get_all_values(LineNo+1, Pos),
    {Pre, Same, Post}.

get_integers(L, {Pre, Same, Post}) ->
    Pos = lists:usort(Pre ++ Same ++ Post),
    Parts = get_parts_from_line(L, Pos, 0),
    LP = search_parts(Parts),
    {undefined, Nums} = combine_parts(LP),
    lists:sum([list_to_integer(N) || N <- Nums]).

combine_parts(LP) ->
    lists:foldl(fun ({[], []}, {undefined, Acc}) ->
                        {undefined, Acc};
                    ({[], []}, {N0, Acc}) ->
                        {undefined, [N0|Acc]};
                    ({[], N0}, {undefined, Acc}) ->
                        {N0, Acc};
                    ({[], N0}, {N1, Acc}) ->
                        {N0, [N1|Acc]};
                    ({N0, []}, {undefined, Acc}) ->
                        {undefined, [N0|Acc]};
                    ({N0, []}, {N1, Acc}) ->
                        {undefined, [N1++N0|Acc]};
                    ({N0, N2}, {undefined, Acc}) ->
                        {N2, [N0|Acc]};
                    ({N0, N2}, {N1, Acc}) ->
                        {N2, [N1++N0|Acc]}
                end, {undefined, []}, LP).
    %% Parts.

get_parts_from_line(_, [], _) ->
    [];
get_parts_from_line(L, [P], F) ->
    P0 = binary:part(L, F, P-F),
    P1 = binary:part(L, P, size(L)-P),
    [P0, P1];
get_parts_from_line(L, [P|Pos], F) ->
    Part = binary:part(L, F, P-F),
    [Part|get_parts_from_line(L, Pos, P)].

search_parts([]) ->
    [];
search_parts([P|Parts]) ->
    M0 = match_first(P),
    M1 = match_last(P),
    M2 = match_first_extra(P),
    case M2 of
        [] ->
            [{M0, M1}|search_parts(Parts)];
        _ ->
            [{[], M2}, {[], M1}|search_parts(Parts)]
    end.

match_first(P) ->
    case re:run(P, "^\([0-9]+\)", [{capture, all_but_first, list}]) of
        nomatch ->
            [];
        {match, [M0]} ->
            M0
    end.
match_first_extra(P) ->
    case re:run(P, "^[^0-9]\([0-9]+\)", [{capture, all_but_first, list}]) of
        nomatch ->
            [];
        {match, [M0]} ->
            M0
    end.
match_last(P) ->
    case re:run(P, "\([0-9]+\)$", [{capture, all_but_first, list}]) of
        nomatch ->
            [];
        {match, [M0]} ->
            M0
    end.
